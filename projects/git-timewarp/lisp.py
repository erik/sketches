""" Extremely simple lisp parser, inspired by http://norvig.com/lispy2.html """

from __future__ import print_function

import cStringIO as StringIO
import operator as op
import os.path
import re
import subprocess
import sys


class Symbol(str):
    interned = {}

    @classmethod
    def intern(cls, name):
        sym = cls.interned.get(name, Symbol(name))
        cls.interned[name] = sym
        return sym

    def __str__(self):
        return '' + self

    def __repr__(self):
        return '#<%s>' % self


class Scope(dict):
    def __init__(self, params=(), args=(), parent=None, git=None):
        self.parent = parent
        self.git_scope = git

        if isinstance(params, Symbol):
            self.update({params: list(args)})

        else:
            assert len(params) == len(args)
            self.update(zip(params, args))

    def get_git_scope(self):
        if self.git_scope is not None:
            return self.git_scope

        if self.parent is not None:
            return self.parent.get_git_scope()

    def find(self, ident):
        if ident in self:
            return self[ident]

        elif self.parent is not None:
            return self.parent.find(ident)

        elif self.git_scope is not None:
            exp = self.git_scope.find(ident)
            return eval_exp(exp, self)

        raise LookupError(ident)


def make_global_scope(repo_path, fname):
    git = GitScope(repo_path, fname)
    global_scope = Scope(git=git)

    global_scope.update({
        '+': lambda *args: sum(args),
        '-': op.sub,
        '*': lambda *args: reduce(lambda a, b: a*b, args, 1),
        '/': op.div,
        'not': op.not_,
        '>': op.gt,
        '<': op.lt,
        '>=': op.ge,
        '<=': op.le,
        '=': op.eq,
        'list': lambda *args: list(args),
        'read': read,
        'eval': lambda exp: eval_exp(exp, global_scope),
        'unparse': unparse
    })

    return global_scope


class GitScope(Scope):
    def __init__(self, repo_path, fname='time.lisp'):
        self.repo = repo_path
        self.file_path = os.path.join(repo_path, fname)
        self.file_name = fname
        self.git = ['git', '-C', self.repo]

    def __repr__(self):
        return '#git{%s}' % self.file_name

    def commit(self, branch, code, message=None):
        subprocess.check_call(self.git + ['checkout', '-B', branch])

        with open(self.file_path, 'w') as fp:
            print('writing', code)
            fp.write(code)

        subprocess.check_call(self.git + ['add', self.file_name])
        subprocess.check_call(self.git + ['commit', '-m', message or branch])

        return branch

    def find(self, treeish):
        print('Entering time machine: %s' % treeish)

        blob = '%s:%s' % (treeish, self.file_name)

        ps = subprocess.Popen(self.git + ['cat-file', 'blob', blob],
                              stdout=subprocess.PIPE)

        out, _ = ps.communicate()

        if out.strip():
            return read(out)


class Lambda(object):
    def __init__(self, params, body, scope):
        self.params, self.body, self.scope = params, body, scope

    def __call__(self, *args):
        call_scope = Scope(self.params, args, self.scope)
        return eval_exp(self.body, call_scope)


EOF = Symbol('#<eof>')


def tokenize(buf):
    tokenizer = r'''^\s*([(')]|"(?:[^"])*"|;.*|[^\s('"`;)]+)(.*)'''

    line = ''

    while True:
        if not buf:
            break

        if line == '':
            line = buf.readline()

        if line == '':
            break

        match = re.match(tokenizer, line)
        if not match:
            break

        token, line = match.groups()
        if token != '' and not token.startswith(';'):
            yield token

    yield EOF


def atom(tok):
    if tok == '#t':
        return True
    elif tok == '#f':
        return False
    elif tok[0] == '"':
        return tok[1:-1].decode('string_escape')

    types = [int, float, Symbol.intern]
    for typ in types:
        try:
            return typ(tok)
        except ValueError:
            pass
    else:
        raise SyntaxError('everything is broken.')


def parse(tokens):
    def handle_tok(tok):
        if tok == '(':
            lst = []

            for tok in tokens:
                if tok == ')':
                    return lst
                elif tok == EOF:
                    raise SyntaxError('expected )')
                else:
                    lst.append(handle_tok(tok))

        elif tok == ')':
            raise SyntaxError('unmatched )')

        elif tok == "'":
            return [Symbol.intern('quote'), handle_tok(next(tokens))]

        elif tok == EOF:
            raise SyntaxError('unexpected EOF')

        else:
            return atom(tok)

    tok = next(tokens)
    return EOF if tok is EOF else handle_tok(tok)


def unparse(ast):
    if isinstance(ast, list):
        return '(%s)' % ' '.join(map(unparse, ast))
    elif isinstance(ast, bool):
        return {True: '#t', False: '#f'}[ast]
    elif isinstance(ast, Symbol):
        return str(ast)
    elif isinstance(ast, str):
        return '"%s"' % ast
    elif isinstance(ast, int) or isinstance(ast, float):
        return str(ast)
    elif isinstance(ast, Lambda):
        params = unparse(ast.params)
        body = unparse(ast.body)

        return '(lambda %s %s)' % (params, body)
    else:
        raise ValueError('what is this:' % repr(ast))


def read(string):
    io = StringIO.StringIO(string)
    return parse(tokenize(io))


def eval_exp(exp, scope):
    if isinstance(exp, Symbol):
        return scope.find(exp)

    # atoms unevaluated
    elif not isinstance(exp, list):
        return exp

    # evaluate function call
    fn_atom, args = exp[0], exp[1:]

    # builtin forms
    if fn_atom is Symbol.intern('if'):
        (cond, then, else_) = args
        branch = then if eval_exp(cond, scope) else else_

        return eval_exp(branch, scope)

    elif fn_atom is Symbol.intern('lambda'):
        (params, body) = args
        return Lambda(params, body, scope)

    elif fn_atom is Symbol.intern('quote'):
        if len(args) == 1:
            return args[0]

        return args

    elif fn_atom is Symbol.intern('do'):
        val = None
        for arg in args:
            val = eval_exp(arg, scope)
        return val

    elif fn_atom is Symbol.intern('print'):
        vals = [eval_exp(arg, scope) for arg in args]
        print(repr(vals))

        return vals

    elif fn_atom is Symbol.intern('stash!'):
        # TODO: this should modify the file and then git stash
        # TODO: (stash! (lambda (x) (x + 1)))
        pass

    elif fn_atom is Symbol.intern('pop!'):
        # TODO: this should pop the stash and return the parsed AST
        # TODO: (eval (pop!))
        pass

    elif fn_atom is Symbol.intern('commit!'):
        # TODO: this should apply a commit on top of the named branch.
        # TODO: (commit! 'fn/add-1 (lambda (x) (x + 1)))

        (branch, code) = map(lambda x: eval_exp(x, scope), args)

        assert isinstance(branch, str)

        git = scope.get_git_scope()
        return git.commit(branch, unparse(code))

    else:
        exps = [eval_exp(e, scope) for e in exp]
        fn, args = exps[0], exps[1:]

        if isinstance(fn, Lambda):
            scope = Scope(fn.params, args, fn.scope)
            return fn(*args)
        else:
            return fn(*args)
