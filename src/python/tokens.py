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

def unrecognized_token(s):
    raise Exception("The string \'%(s)s\' cannot be tokenized." % locals())

# this is the tokenizer for basic SOCP problems
scanner = re.Scanner([
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
        (r"0+|0+\.0*",              lambda scanner,token:("ZEROS", e.Constant(0.0)) ),
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

# "operator" precedence
op_prec = {'ABS':4, 'NORM': 4, 'UMINUS':2, 'MULT_OP':3, 'PLUS_OP':1, 'MINUS_OP':1, 'EQ':0, 'GEQ':0, 'LEQ':0, \
            '':0, 'LPAREN':0}   # these last two are to help us get unary minus and plus

def precedence(tok):
    return op_prec.get(tok, -1)