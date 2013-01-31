import re
import expression as e

def unrecognized_token(s):
    raise Exception("The string \'%(s)s\' cannot be tokenized." % locals())

# this is the tokenizer for basic SOCP problems    
scanner=re.Scanner([
    (r"#.*",                    None ), # gobble comments
    (r"variable ",              lambda scanner,token:("VARIABLE", token) ),
    (r"parameter ",             lambda scanner,token:("PARAMETER", token) ),
    (r"vector",                 lambda scanner,token:("VECTOR", token) ),
    (r"scalar",                 lambda scanner,token:("SCALAR", token) ),
    (r"matrix",                 lambda scanner,token:("MATRIX", token) ),
    (r"positive|nonnegative",   lambda scanner,token:("POSITIVE", token) ),
    (r"negative|nonpositive",   lambda scanner,token:("NEGATIVE", token) ),
    (r"minimize ",              lambda scanner,token:("MINIMIZE", token) ),
    (r"maximize ",              lambda scanner,token:("MAXIMIZE", token) ),
    (r"find ",                  lambda scanner,token:("FIND", token) ),
    (r"subject to",             lambda scanner,token:("SUBJECT_TO", token) ),
    (r"==",                     lambda scanner,token:("EQ", token) ),
    (r"<=",                     lambda scanner,token:("LEQ", token) ),
    (r">=",                     lambda scanner,token:("GEQ", token) ),
    (r"0+|0+\.0*",              lambda scanner,token:("ZEROS", e.Constant(float(token))) ),
    (r"ones",                   lambda scanner,token:("ONES", e.Constant(1.0)) ),
    # abs and norm are built in for the second-order cones
    (r"abs",                    lambda scanner,token:("ABS", token) ),
    (r"norm",                   lambda scanner,token:("NORM", token) ),
    (r"\d+\.\d*",               lambda scanner,token:("CONSTANT", e.Constant(float(token))) ),
    (r"\d+",                    lambda scanner,token:("CONSTANT", e.Constant(float(token))) ),
    (r"\(",                     lambda scanner,token:("LPAREN", token) ),
    (r"\)",                     lambda scanner,token:("RPAREN", token) ),
    (r"[a-zA-Z_\d]+",  	        lambda scanner,token:("IDENTIFIER", token) ),
    (r"\+",                     lambda scanner,token:("PLUS_OP", token) ),
    (r"\-",                     lambda scanner,token:("MINUS_OP", token) ),
    (r"\*",                     lambda scanner,token:("MULT_OP", token) ),
    (r",",                      lambda scanner,token:("COMMA", token) ),
    (r"\s+",                    None), # None == skip token.
    (r".",                      lambda scanner,token:unrecognized_token(token))
])