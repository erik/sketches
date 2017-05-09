CUT_CHARS = {'(', ')', ' '}
LITERAL_TOKENS = {'(', ')'}


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
        return value, rest
    elif tok == 'ident':
        return value, rest
    else:
        raise SyntaxError('unexpected token: %s: %s' % (tok, value))


def parse_list(tokens):
    lst = []

    while len(tokens):
        (tok, value) = tokens[0]
        if tok == ')':
            return lst, tokens[1:]
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


if __name__ == '__main__':
    pass
