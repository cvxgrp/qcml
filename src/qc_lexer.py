"""
The design of this module borrows heavily from the PLY manual and the
pycparser library.
"""
import operator
from ply import lex
from . ast.atoms import atoms


class QCLexer:
    def __init__(self):
        self.dimensions = set()
        self.variables = {}
        self.parameters = {}

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
        ('subject', 'SUBJ'),
        ('to', 'TO'),
        # builtin functions in the language
        ('sum', 'SUM'),
    ] + zip(atoms.keys(),['ATOM']*len(atoms.keys())))

    tokens = [
        'INTEGER','CONSTANT', 'PLUS', 'MINUS', 'TIMES', 'DIVIDE', 'EQ', 'LEQ', 'GEQ',
        'COMMA', 'TRANSPOSE', 'LPAREN', 'RPAREN',
        'ID', 'DIM_ID', 'VAR_ID', 'PARAM_ID', 'COMMENT', 'NL'
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
    # t_SEMI      = r';'
    t_TRANSPOSE = r'\''
    # t_LBRACE    = r'\['
    # t_RBRACE    = r'\]'
    t_LPAREN    = r'\('
    t_RPAREN    = r'\)'

    # for parsing constant floats
    # WARNING: this must appear before t_INTEGER
    def t_CONSTANT(self,t):
        r'\d+\.\d*'
        t.value = float(t.value)
        return t

    # for parsing integers
    def t_INTEGER(self,t):
        r'\d+'
        t.value = int(t.value)
        return t

    # for identifiers
    def t_ID(self,t):
        r'[a-zA-Z][a-zA-Z_0-9]*'
        t.type = self.reserved.get(t.value, 'ID')
        if t.type == 'ID':
            # check to see if it's a dimension, variable, or parameter id
            if t.value in self.dimensions:
                t.type = 'DIM_ID'
                return t

            new_value = self.variables.get(t.value, None)
            if new_value is not None:
                t.type = 'VAR_ID'
                t.value = new_value
                return t

            new_value = self.parameters.get(t.value, None)
            if new_value is not None:
                t.type = 'PARAM_ID'
                t.value = new_value
                return t

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

