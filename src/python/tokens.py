"""
Copyright (c) 2012-2013, Eric Chu (eytchu@gmail.com)
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met: 

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer. 
2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution. 

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.

The views and conclusions contained in the software and documentation are
those of the authors and should not be interpreted as representing official
policies, either expressed or implied, of the FreeBSD Project.
"""

import re
import expression as e
import operator
from atoms import macros, abs_, norm, sum_

def unrecognized_token(s):
    raise Exception("The string \'%(s)s\' cannot be tokenized." % locals())
        
def macro_or_identifier(mangle, token):
    f = macros.get(token, None)
    if f: return ("MACRO", f)
    elif mangle: return ("IDENTIFIER", '_' + token)
    else: return ("IDENTIFIER", token)

# this is the tokenizer for basic SOCP problems
def create_scanner(mangle):
    return re.Scanner([
        (r"#.*",                    None ), # gobble comments
        (r"variable(?=\s+)",        lambda scanner,token:("VARIABLE", token) ),
        (r"parameter(?=\s+)",       lambda scanner,token:("PARAMETER", token) ),
        (r"vector",                 lambda scanner,token:("VECTOR", token) ),
        (r"scalar",                 lambda scanner,token:("SCALAR", token) ),
        (r"matrix",                 lambda scanner,token:("MATRIX", token) ),
        (r"positive|nonnegative",   lambda scanner,token:("POSITIVE", token) ),
        (r"negative|nonpositive",   lambda scanner,token:("NEGATIVE", token) ),
        (r"minimize(?=\s+)",        lambda scanner,token:("MINIMIZE", token) ),
        (r"maximize(?=\s+)",        lambda scanner,token:("MAXIMIZE", token) ),
        (r"find(?=\s+)",            lambda scanner,token:("FIND", token) ),
        (r"subject to",             lambda scanner,token:("SUBJECT_TO", token) ),
        (r"==",                     lambda scanner,token:("EQ", operator.eq) ),
        (r"<=",                     lambda scanner,token:("LEQ", operator.le) ),
        (r">=",                     lambda scanner,token:("GEQ", operator.ge) ),
        (r"\d+\.\d*",               lambda scanner,token:("CONSTANT", e.Constant(float(token))) ),
        (r"\d+",                    lambda scanner,token:("CONSTANT", e.Constant(float(token))) ),
        (r"0+|0+\.0*",              lambda scanner,token:("ZEROS", e.Constant(0.0)) ),
        (r"ones",                   lambda scanner,token:("ONES", e.Constant(1.0)) ),
        # abs and norm are built in for the second-order cones
        (r"abs(?=\()",              lambda scanner,token:("ABS", abs_) ),
        (r"norm2(?=\()|norm(?=\()", lambda scanner,token:("NORM", norm) ),
        (r"sum(?=\()",              lambda scanner,token:("SUM", sum_) ),
        (r"\(",                     lambda scanner,token:("LPAREN", token) ),
        (r"\)",                     lambda scanner,token:("RPAREN", token) ),
        (r"\[",                     lambda scanner,token:("LBRACE", token) ),
        (r"\]",                     lambda scanner,token:("RBRACE", token) ),
        (r"[a-zA-Z_\d]+",  	        lambda scanner,token:macro_or_identifier(mangle, token) ),
        (r"\+",                     lambda scanner,token:("PLUS_OP", operator.add) ),
        (r"\-",                     lambda scanner,token:("UMINUS", operator.neg) ),
        (r"\*",                     lambda scanner,token:("MULT_OP", operator.mul) ),
        (r",",                      lambda scanner,token:("COMMA", token) ),
        (r";",                      lambda scanner,token:("SEMI", token) ),
        (r"\s+",                    None), # None == skip token.
        (r".",                      lambda scanner,token:unrecognized_token(token))
    ])

scanner_mangled = create_scanner(True)
scanner_unmangled = create_scanner(False)

# "operator" precedence
op_prec = { 'ABS':4, 'NORM': 4, 'SUM': 4, 'MULT_OP':3,  'UMINUS':2, \
            'PLUS_OP':1, 'MINUS_OP':1, 'EQ':0, 'GEQ':0, 'LEQ':0, \
            '':0, 'LPAREN':0, 'LBRACE':0}   # these last three are to help us get unary minus and plus

def precedence(tok):
    if tok is "MACRO":
        return 4
    else:
        return op_prec.get(tok, -1)

