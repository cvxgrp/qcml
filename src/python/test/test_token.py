import qcml.qc_lexer as s


lexer = s.QCLexer()
lexer.build()
lex = lexer.lexer

# these should lex properly, but may be nonsensicla
test_strings = [
    "minimize A*x + b - c' + square(x)",
    "maximize minimize parameter nonnegative nonpositive",
    "subject to 3*2 - 1 ==<=>= 0.3 0",
    "variable m negative parameter A positive",
    "norm ( ) norm(x) norm2(x) norm1 (x)",
    "[--total geo_mean(k,s) ;]",
    "",
    "parameter xvectorpositive",
    "parameter x vectorpositive",
    "variable x#hello",
    "dimensions a b c variables d e f(n)"
]

# these might fail
fail_strings = [
    "<=="
]

# expected success tokens
test_tokens = [
    ["SENSE", "ID", "TIMES", "ID", "PLUS", "ID",
     "MINUS", "ID", "TRANSPOSE", "PLUS", "ATOM", "LPAREN", "ID", "RPAREN"],
    ["SENSE", "SENSE", "PARAMETER", "SIGN", "SIGN"],
    ["SUBJ", "TO", "INTEGER", "TIMES", "INTEGER", "MINUS", "INTEGER", "EQ", "LEQ", "GEQ", "CONSTANT", "INTEGER"],
    ["VARIABLE", "ID", "SIGN", "PARAMETER", "ID", "SIGN"],
    ["ATOM", "LPAREN", "RPAREN", "ATOM", "LPAREN", "ID", "RPAREN", "ATOM", "LPAREN", "ID", "RPAREN", "ATOM", "LPAREN", "ID", "RPAREN"],
    ["LBRACE", "MINUS", "MINUS", "ID", "ATOM", "LPAREN", "ID", "COMMA", "ID", "RPAREN", "SEMI", "RBRACE"],
    [],
    ["PARAMETER", "ID"],
    ["PARAMETER", "ID", "ID"],
    ["VARIABLE", "ID"],
    ["DIMENSIONS", "ID", "ID", "ID", "VARIABLES", "ID", "ID", "ID", "LPAREN", "ID", "RPAREN"]
]

# expected identifiers
test_id = [
    ["A","x", "b", "c", "x"],
    [],
    [],
    ["m", "A"],
    ["x", "x", "x"],
    ["total", "k", "s"],
    [],
    ["xvectorpositive"],
    ["x", "vectorpositive"],
    ["x"]
]

# expected atoms
test_atoms = [
    ['square'],
    [],
    [],
    [],
    ['norm', 'norm', 'norm2', 'norm1'],
    ['geo_mean'],
    [],
    [],
    [],
    []
]

def test_scanner():
    for s,exp in zip(test_strings, test_tokens):
        yield check_lex, s, exp
    for s,exp in zip(test_strings, test_id):
        yield check_id, s, exp
    for s,exp in zip(test_strings, test_atoms):
        yield check_atom, s, exp


def check_lex(s, expected):
    lex.input(s)

    tok_list = []
    while True:
        tok = lex.token()
        if not tok: break
        print tok
        tok_list.append(tok)

    assert( all(t.type == e for t,e in zip(tok_list, expected)) )
    #assert(not remainder)

def check_id(s, expected):
    lex.input(s)

    tok_list = []
    while True:
        tok = lex.token()
        if not tok: break
        tok_list.append(tok)

    ids = filter(lambda x: x.type == "ID", tok_list)
    print ids
    assert( all(t.value == e for t,e in zip(ids, expected)) )

def check_atom(s, expected):
    lex.input(s)

    tok_list = []
    while True:
        tok = lex.token()
        if not tok: break
        tok_list.append(tok)

    ids = filter(lambda x: x.type == "ATOM", tok_list)
    print ids
    assert( all(t.value == e for t,e in zip(ids, expected)) )