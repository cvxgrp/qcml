"""
The design of this module borrows heavily from the PLY manual and the
pycparser library.
"""
import operator
from ply import lex
from . ast.atoms import atoms


class QCLexer(object):
    def build(self, **kwargs):
        """ Builds the lexer from the specification. Must be
            called after the lexer object is created.

            This method exists separately, because the PLY
            manual warns against calling lex.lex inside
            __init__
        """
        self.lexer = lex.lex(module=self, **kwargs)

    # reserved keywords in the language
    reserved = dict([
        ('variable', 'VARIABLE'),
        ('parameter', 'PARAMETER'),
        ('dimension', 'DIMENSION'),
        #'expression'  : 'EXPRESSION',
        ('variables', 'VARIABLES'),
        ('parameters', 'PARAMETERS'),
        ('dimensions', 'DIMENSIONS'),
        ('dual', 'DUAL'),   # modifier for "variable"
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
        'COMMA', 'TRANSPOSE', 'LPAREN', 'RPAREN', 'COLON',
        'ID', 'NL'
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
    t_COLON     = r':'
    # t_SEMI      = r';'
    t_TRANSPOSE = r'\''
    # t_LBRACE    = r'\['
    # t_RBRACE    = r'\]'
    t_LPAREN    = r'\('
    t_RPAREN    = r'\)'

    # for parsing constant floats
    # WARNING: this must appear before t_INTEGER
    def t_CONSTANT(self, tok):
        r'\d+\.\d*'
        tok.value = float(tok.value)
        return tok

    # for parsing integers
    def t_INTEGER(self, tok):
        r'\d+'
        tok.value = int(tok.value)
        return tok

    # for identifiers
    def t_ID(self, tok):
        r'[a-zA-Z][a-zA-Z_0-9]*'
        tok.type = self.reserved.get(tok.value, 'ID')
        # if tok.type == 'ID' and self.is_lex_expr:
        #     # check to see if it's a dimension, variable, or parameter id
        #     if tok.value in self.dimensions:
        #         tok.type = 'DIM_ID'
        #         return tok
        #
        #     new_value = self.variables.get(tok.value, None)
        #     if new_value is not None:
        #         tok.type = 'VAR_ID'
        #         tok.value = new_value
        #         return tok
        #
        #     new_value = self.parameters.get(tok.value, None)
        #     if new_value is not None:
        #         tok.type = 'PARAM_ID'
        #         tok.value = new_value
        #         return tok

        return tok

    # newline is used as an ending
    def t_NL(self, tok):
        r'\n+'
        tok.lexer.lineno += tok.value.count("\n")
        return tok

    # comment token
    def t_COMMENT(self, tok):
        r'\#[^\\\n]*'
        pass

    # things to ignore (spaces and tabs)
    t_ignore = ' \t'

    # error handling
    def t_error(self, tok):
        print "QC_LEX: Illegal character '%s'" % tok.value[0]
        tok.lexer.skip(1)
