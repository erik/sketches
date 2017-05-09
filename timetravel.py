CUT_CHARS = {'(', ')', ' '}
LITERAL_TOKENS = {'(', ')'}
NODE_TYPES = {'if', 'fn', 'builtin'}
BUILTINS = {'list', 'print'}


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

    def evaluate_exp(self, exp):
        kind, value = exp

        if kind == 'list':
            return self.evaluate_list(value)
        elif kind == 'number':
            return value
        elif kind == 'ident':
            return self.lookup_ident(value)
        else:
            # not implemented...
            raise NotImplemented('oops')

    def evaluate_list(self, lst):
        if not lst:
            return

        fn, args = lst[0], lst[1:]
        return self.evaluate_function(fn, args)

    def evaluate_function(self, fn, args):
        kind, ident = fn

        if kind == 'builtin':
            return self.evaluate_builtin(ident, args)

        elif kind == 'if':
            assert len(args) == 3, '(if cond then else)'
            cond, then, else_ = args

            if self.evaluate_exp(cond):
                return self.evaluate_exp(then)

            else:
                return self.evaluate_exp(else_)

    def evaluate_builtin(self, fn, args):
        if fn == 'list':
            return args

        elif fn == 'print':
            print args

    def lookup_ident(self, ident):
        if ident in BUILTINS:
            return ('builtin', ident)


if __name__ == '__main__':
    pass
