from scoop.tokens import scanner, precedence
from scoop.atoms import macros

# these should lex properly, but may be nonsensicla
test_strings = [
    "minimize A*x + b - c' + square(x)",
    "maximize find parameter nonnegative nonpositive",
    "subject to 3*2 - 1 ==<=>= 0.3 0",
    "variable m scalar negative parameter matrix A positive",
    "norm ( ) norm(x) norm2(x) norm1 (x)",
    "[--total geo_mean(k,s) ;]",
    "",
    "parameter xvectorpositive",
    "parameter x vectorpositive",
    "variable x vector#hello"
]

# these might fail
fail_strings = [
    "<=="
]

# expected success tokens
test_tokens = [
    ["MINIMIZE", "IDENTIFIER", "MULT_OP", "IDENTIFIER", "PLUS_OP", "IDENTIFIER",
     "UMINUS", "IDENTIFIER", "TRANSPOSE", "PLUS_OP", "MACRO", "LPAREN", "IDENTIFIER", "RPAREN"],
    ["MAXIMIZE", "FIND", "PARAMETER", "POSITIVE", "NEGATIVE"],
    ["SUBJECT_TO", "CONSTANT", "MULT_OP", "CONSTANT", "UMINUS", "CONSTANT", "EQ", "LEQ", "GEQ", "CONSTANT", "CONSTANT"],
    ["VARIABLE", "IDENTIFIER", "SCALAR", "NEGATIVE", "PARAMETER", "MATRIX", "IDENTIFIER", "POSITIVE"],
    ["MACRO", "LPAREN", "RPAREN", "MACRO", "LPAREN", "IDENTIFIER", "RPAREN", "MACRO", "LPAREN", "IDENTIFIER", "RPAREN", "MACRO", "LPAREN", "IDENTIFIER", "RPAREN"],
    ["LBRACE", "UMINUS", "UMINUS", "IDENTIFIER", "MACRO", "LPAREN", "IDENTIFIER", "COMMA", "IDENTIFIER", "RPAREN", "SEMI", "RBRACE"],
    [],
    ["PARAMETER", "IDENTIFIER"],
    ["PARAMETER", "IDENTIFIER", "IDENTIFIER"],
    ["VARIABLE", "IDENTIFIER", "VECTOR"]
]

# expected identifiers
test_id = [
    ["_A","_x", "_b", "_c", "_x"],
    [],
    [],
    ["_m", "_A"],
    ["_x", "_x", "_x"],
    ["_total", "_k", "_s"],
    [],
    ["_xvectorpositive"],
    ["_x", "_vectorpositive"],
    ["_x"]
]

# expected macros
test_macros = [
    [macros['square']],
    [],
    [],
    [],
    [macros['norm'], macros['norm'], macros['norm2'], macros['norm1']],
    [macros['geo_mean']],
    [],
    [],
    [],
    []
]

def test_scanner():
    for s,exp in zip(test_strings, test_tokens):
        yield check_lex, scanner.scan, s, exp
    for s,exp in zip(test_strings, test_id):
        yield check_id, scanner.scan, s, exp
    for s,exp in zip(test_strings, test_macros):
        yield check_macro, scanner.scan, s, exp
    

def check_lex(f, s, expected):
    tok_list, remainder = f(s)
    print tok_list
    assert(not remainder)
    assert( all(t[0] == e for t,e in zip(tok_list, expected)) )

def check_id(f, s, expected):
    tok_list, remainder = f(s)
    print tok_list
    print s
    print expected
    assert(not remainder)
    ids = filter(lambda x: x[0] is "IDENTIFIER", tok_list)
    assert( all(t[1] == e for t,e in zip(ids, expected)) )

def check_macro(f, s, expected):
    tok_list, remainder = f(s)
    print tok_list
    assert(not remainder)
    ids = filter(lambda x: x[0] is "MACRO", tok_list)
    assert( all(t[1] is e for t,e in zip(ids, expected)) )