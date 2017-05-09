import sys

CUT_CHARS = {'(', ')', ' '}
LITERAL_TOKENS = {'(', ')'}
NODE_TYPES = {'if', 'fn', 'builtin'}
BUILTINS = {'map', 'list', 'print', '+', '-', '*', '/'}
CALLABLE_TYPES = {'fn', 'fn_literal', 'builtin'}


def lex(source):
    i = 0

    def ident_or_num(i):
        string = ''

        while i < len(source) and source[i] not in CUT_CHARS:
            string += source[i]
            i += 1

        return (i, string)

    while i < len(source):
        if source[i] in LITERAL_TOKENS:
            i += 1
            yield (source[i-1], None)
        elif source[i] == ' ':
            i += 1
            continue
        else:
            (i, tok) = ident_or_num(i)

            if tok.isdigit():
                yield ('number', int(tok))

            else:
                yield ('ident', tok)

    yield ('eof', None)


def parse_exp(tokens):
    (tok, value), rest = tokens[0], tokens[1:]
    if tok == '(':
        return parse_list(rest)
    elif tok == 'number':
        return ('number', value), rest
    elif tok == 'ident' and value in BUILTINS:
        return ('builtin', value), rest
    elif tok == 'ident' and value in NODE_TYPES:
        return (value, value), rest
    elif tok == 'ident' and value not in NODE_TYPES:
        return ('ident', value), rest
    else:
        raise SyntaxError('unexpected token: %s: %s' % (tok, value))


def parse_list(tokens):
    lst = []

    while len(tokens):
        (tok, value) = tokens[0]
        if tok == ')':
            return ('list', lst), tokens[1:]
        elif tok == 'eof':
            raise SyntaxError('unexpected EOF')
        else:
            value, tokens = parse_exp(tokens)
            lst.append(value)

    raise SyntaxError('unterminated list?')


def parse(source):
    tokens = [t for t in lex(source)]
    ast = []

    while len(tokens):
        if tokens[0][0] == 'eof':
            break

        (value, tokens) = parse_exp(tokens)
        ast.append(value)

    return ast


class Interpreter(object):
    def __init__(self, ast):
        self.commitish = {}
        self.ast = ast

    def evaluate(self):
        result = None

        for kind, value in self.ast:
            print 'eval:', kind, value

            result = self.evaluate_exp((kind, value))

        return result

    def evaluate_exp(self, exp, bindings={}):
        print 'eval exp', exp, bindings
        kind, value = exp

        if kind == 'list':
            return self.evaluate_list(value, bindings)
        elif kind == 'number':
            return exp

        elif kind == 'ident':
            return self.lookup_ident(value, bindings)

        elif kind in {'fn', 'fn_literal', 'builtin'}:
            return exp

        else:
            # not implemented...
            raise NotImplemented('oops')

    def evaluate_list(self, lst, bindings={}):
        if not lst:
            return

        (kind, val), args = lst[0], lst[1:]
        (kind_, val_) = self.evaluate_exp((kind, val), bindings)
        print 'evaluated->', kind_, val_
        assert kind_ in CALLABLE_TYPES, 'can only evaluate function!'

        return self.evaluate_function((kind_, val_), args, bindings)

    def evaluate_function(self, fn, args, bindings={}):
        kind, ident = fn

        if kind == 'builtin':
            return self.evaluate_builtin(ident, args, bindings)

        elif kind == 'if':
            assert len(args) == 3, '(if cond then else)'
            cond, then, else_ = args

            if self.evaluate_exp(cond):
                return self.evaluate_exp(then, bindings)

            else:
                return self.evaluate_exp(else_, bindings)

        elif kind == 'fn':
            print 'build_fn:', args
            assert len(args) == 2, '(fn (arg, ...) body)'
            assert args[0][0] == 'list', 'expected args to be list of ident'
            assert all(a[0] == 'ident' for a in args[0][1])

            fn_args = [v for (_, v) in args[0][1]]

            return ('fn_literal', {'args': fn_args, 'body': args[1]})

        elif kind == 'fn_literal':
            assert len(args) == len(ident['args']), 'incorrect arg count'

            args_ = [self.evaluate_exp(a, bindings) for a in args]
            bindings = dict(zip(ident['args'], args_))

            return self.evaluate_exp(ident['body'], bindings=bindings)

    def evaluate_builtin(self, fn, args, bindings={}):
        args = [self.evaluate_exp(a, bindings) for a in args]

        if fn == 'list':
            return args

        elif fn == 'print':
            print args

        elif fn == 'map':
            fn_ = args[0]
            return self.evaluate_function(fn_, args[1:], bindings)

        elif fn == '+':
            return sum(args)

        elif fn == '*':
            return reduce(lambda a, b: a*b, args, 1)

        else:
            raise NotImplemented('sorry')

    def lookup_ident(self, ident, bindings):
        if ident in BUILTINS:
            return ('builtin', ident)
        if ident in bindings:
            return bindings[ident]


if __name__ == '__main__':
    repo = sys.argv[1]
    print 'running on repo:', repo
