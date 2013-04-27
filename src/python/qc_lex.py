"""
The design of this module borrows heavily from the PLY manual and the
pycparser library.
"""
# import re
# import expression as e
import operator
from qc_ply import lex
from qc_atoms import atoms
# from atoms import macros, abs_, norm, sum_


class QCLexer:
    def __init__(self): # does nothing yet
        pass
        
    def build(self, **kwargs):
        """ Builds the lexer from the specification. Must be
            called after the lexer object is created. 
            
            This method exists separately, because the PLY
            manual warns against calling lex.lex inside
            __init__
        """
        self.lexer = lex.lex(module=self, **kwargs)
        
    # Test it output
    def test(self,data):
        self.lexer.input(data)
        while True:
             tok = self.lexer.token()
             if not tok: break
             print tok
    
    # reserved keywords in the language
    reserved = dict([
        ('variable', 'VARIABLE'),
        ('parameter', 'PARAMETER'),
        ('dimension', 'DIMENSION'),
        #'expression'  : 'EXPRESSION',
        ('variables', 'VARIABLES'),
        ('parameters', 'PARAMETERS'),
        ('dimensions', 'DIMENSIONS'),
        #'expressions' : 'EXPRESSIONS',
        ('positive', 'SIGN'),
        ('negative', 'SIGN'),
        ('nonnegative', 'SIGN'),
        ('nonpositive', 'SIGN'),
        ('minimize', 'SENSE'),
        ('maximize', 'SENSE'),
        ('find', 'SENSE'),
        ('subject', 'SUBJ'),
        ('to', 'TO'),
        # builtin functions in the language
        ('norm', 'NORM'),
        ('norm2', 'NORM'),
        ('abs', 'ABS'),
        ('sum', 'SUM'),
        #'norms' : 'NORM',
    ] + zip(atoms.keys(),['ATOM']*len(atoms.keys())))

    tokens = [
        'INTEGER','CONSTANT', 'PLUS', 'MINUS', 'TIMES', 'DIVIDE', 'EQ', 'LEQ', 'GEQ',
        'COMMA', 'SEMI', 'TRANSPOSE', 'LBRACE', 'RBRACE', 'LPAREN', 'RPAREN',
        'ID', 'COMMENT', 'NL'
    ] + list(set(reserved.values()))

    t_PLUS      = r'\+'
    t_MINUS     = r'\-'
    t_TIMES     = r'\*'
    t_DIVIDE    = r'/'
    # t_ASSIGN    = r'='
    t_EQ        = r'=='
    t_LEQ       = r'<='
    t_GEQ       = r'>='
    t_COMMA     = r','
    t_SEMI      = r';'
    t_TRANSPOSE = r'\''
    t_LBRACE    = r'\['
    t_RBRACE    = r'\]'
    t_LPAREN    = r'\('
    t_RPAREN    = r'\)'

    # for parsing integers
    def t_INTEGER(self,t):
        r'\d+'
        t.value = int(t.value)
        return t
        
    # for parsing constant floats
    def t_CONSTANT(self,t):
        r'\d+\.\d*'
        t.value = float(t.value)
        return t
    


    # for identifiers
    def t_ID(self,t):
        r'[a-zA-Z_][a-zA-Z_0-9]*'
        t.type = self.reserved.get(t.value, 'ID')
        return t

    def t_COMMENT(self,t):
        r'\#.*'
        pass
        # comment token

    # newline rule
    def t_NL(self,t):
        r'\n+'
        t.lexer.lineno += len(t.value)
        return t

    # things to ignore (spaces and tabs)
    t_ignore = ' \t'

    # error handling
    def t_error(self,t):
        print "QC_LEX: Illegal character '%s'" % t.value[0]
        t.lexer.skip(1)
        


    # lexer.input("3.2 + 5")
    # print_lex()
    # lexer.input("x = 4")
    # print_lex()
    # lexer.input("y == atoms(iwer)")
    # print_lex()
    # lexer.input("variable x(n)")
    # print_lex()

    # def unrecognized_token(s):
    #     raise Exception("The string \'%(s)s\' cannot be tokenized." % locals())
    #         
    # def macro_or_identifier(token):
    #     f = macros.get(token, None)
    #     if f: return ('MACRO', f)
    #     else: return ('IDENTIFIER', '_' + token)

    # # this is the tokenizer for basic SOCP problems
    # scanner = re.Scanner([
    #         (r"#.*",                    None ), # gobble comments
    #         (r"variable(?=\s+|$|#)",    lambda scanner,token:('VARIABLE', token) ),
    #         (r"parameter(?=\s+|$|#)",   lambda scanner,token:('PARAMETER', token) ),
    #         (r"vector(?=\s+|$|#)",      lambda scanner,token:('VECTOR', token) ),
    #         (r"scalar(?=\s+|$|#)",      lambda scanner,token:('SCALAR', token) ),
    #         (r"matrix(?=\s+|$|#)",      lambda scanner,token:('MATRIX', token) ),
    #         (r"positive(?=\s+|$|#)|nonnegative(?=\s+|$|#)",   
    #                                     lambda scanner,token:('POSITIVE', token) ),
    #         (r"negative(?=\s+|$|#)|nonpositive(?=\s+|$|#)",   
    #                                     lambda scanner,token:('NEGATIVE', token) ),
    #         (r"minimize(?=\s+|$|#)",    lambda scanner,token:('MINIMIZE', token) ),
    #         (r"maximize(?=\s+|$|#)",    lambda scanner,token:('MAXIMIZE', token) ),
    #         (r"find(?=\s+|$|#)",        lambda scanner,token:('FIND', token) ),
    #         (r"subject to",             lambda scanner,token:('SUBJECT_TO', token) ),
    #         (r"==",                     lambda scanner,token:('EQ', operator.eq) ),
    #         (r"<=",                     lambda scanner,token:('LEQ', operator.le) ),
    #         (r">=",                     lambda scanner,token:('GEQ', operator.ge) ),
    #         (r"\d+\.\d*|\d+",           lambda scanner,token:('CONSTANT', e.Constant(float(token))) ),
    #         (r"\(",                     lambda scanner,token:('LPAREN', token) ),
    #         (r"\)",                     lambda scanner,token:('RPAREN', token) ),
    #         (r"\[",                     lambda scanner,token:('LBRACE', token) ),
    #         (r"\]",                     lambda scanner,token:('RBRACE', token) ),
    #         (r"[a-zA-Z_\d]+",              lambda scanner,token:macro_or_identifier(token) ),
    #         (r"\+",                     lambda scanner,token:('PLUS_OP', operator.add) ),
    #         (r"\-",                     lambda scanner,token:('UMINUS', operator.neg) ),
    #         (r"\*",                     lambda scanner,token:('MULT_OP', operator.mul) ),
    #         (r",",                      lambda scanner,token:('COMMA', token) ),
    #         (r";",                      lambda scanner,token:('SEMI', token) ),
    #         (r"'",                      lambda scanner,token:('TRANSPOSE', e.transpose) ),
    #         (r"\s+",                    None), # None == skip token.
    #         (r".",                      lambda scanner,token:unrecognized_token(token))
    #     ])
    # 
    # # "operator" precedence
    # op_prec = { 'MACRO': 4, 'MULT_OP':3,  'UMINUS':2, \
    #             'PLUS_OP':1, 'MINUS_OP':1, 'EQ':0, 'GEQ':0, 'LEQ':0, \
    #             '':0, 'LPAREN':0, 'LBRACE':0}   # these last three are to help us get unary minus and plus
    # 
    # def precedence(tok):
    #     return op_prec.get(tok, -1)

