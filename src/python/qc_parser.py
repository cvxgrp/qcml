from ply import yacc
from qc_lex import QCLexer

class QCParser(object):
    """ QCParser parses QCML but does not check DCP compliance.
    """
    def __init__(self):
        self.lex = QCLexer();
        self.lex.build();
        self.tokens = self.lex.tokens
        self.parser = yacc.yacc(module = self)
        
        self._dimensions = set()
        self._variables = set()
        self._parameters = set()
        
    def parse(self, text):
        """ Parses QCML and returns an AST.
        
            text:
                A string containing QCML source
        
            XXX / note to self: the AST is traversed afterwards to be
            rewritten. a problem is just a collection of ASTs
        """
        
        # append a newline if one doesn't exist at the end
        if('\n' != text[-1]):
            text += '\n'
        return self.parser.parse(text, debug=False)
    
    def _print_err(self, msg, offset=1):
        """ Prints a QCML parse error.
        
            msg:
                A string containing the message we want to print.
            
            offset:
                An integer for the line offset
        """
        s = self.lex.lexer.lexdata.split('\n')
        num = self.lex.lexer.lineno - offset
        print ">> line %s: %s" % (num+1, s[num].lstrip().rstrip())
        print msg
        
    precedence = (
        ('left', 'PLUS', 'MINUS'),
        ('left', 'TIMES', 'DIVIDE'),
        ('right', 'UMINUS')
    )
    
    def p_program(self,p):
        """program : lines
                   | empty"""
        p[0] = p[1]
    
    def p_lines_line(self,p):
        """lines : declaration NL"""
        if(p[1] is not None):
            p[0] = [p[1]]
    
    def p_lines_many_line(self,p):
        'lines : lines declaration NL'
        if(p[1] is not None and p[2] is not None):
            p[0] = p[1] + [p[2]]
        elif(p[1] is None and p[2] is not None):
            p[0] = [p[2]]
        elif(p[1] is not None and p[2] is None):
            p[0] = p[1]
        else:
            pass
    
    def p_declaration(self,p):
        """declaration : create 
                       | expression
                       | empty
        """
        p[0] = p[1]
        
    
    def p_empty(self,p):
        'empty : '
        pass
    
    def p_create_identifier(self,p):
        """create : VARIABLE array
                  | VARIABLE ID
                  | PARAMETER array
                  | PARAMETER ID
                  | DIMENSION ID
        """
        if(p[1] == 'variable'):
            self._variables.add(p[2])
        if(p[1] == 'parameter'):
            self._parameters.add(p[2])
        if(p[1] == 'dimension'):
            self._dimensions.add(p[2])
    
    def p_array_identifier(self,p):
        'array : ID LPAREN dimlist RPAREN'
        p[0] = p[1]
    
    # (for shape) id, id, id ...
    def p_dimlist_list(self,p):
        'dimlist : dimlist COMMA ID'
        if(p[3] in self._dimensions):
            p[0] = p[1] + [p[2]]
        else:
            self._print_err("dimension %s not declared" % p[2])
    
    def p_dimlist_id(self,p):
        'dimlist : ID'
        if(p[1] in self._dimensions):
            p[0] = [p[1]]
        else:
            self._print_err("dimension %s not declared" % p[1])
    
    # for dimensions
    def p_create_dimensions(self,p):
        'create : DIMENSIONS idlist'
        self._dimensions = self._dimensions.union(p[2])
    
    # (for dimensions) id id id ...
    def p_idlist_list(self,p):
        'idlist : idlist ID'
        p[0] = p[1] + [p[2]]
    
    def p_idlist_id(self,p):
        'idlist : ID'
        p[0] = [p[1]]
        
    def p_expression_binop(self,p):
        '''expression : expression PLUS expression
                      | expression MINUS expression
                      | expression TIMES expression
                      | expression DIVIDE expression
        '''
        p[0] = ('binary-expression', p[2],p[1],p[3])
    
    def p_expression_group(self,p):
        'expression : LPAREN expression RPAREN'
        p[0] = ('group-expression', p[2])
    
    def p_expression_constant(self,p):
        'expression : CONSTANT'
        p[0] = ('constant-expression', p[1])
    
    # (Super ambiguous) error rule for syntax errors
    def p_error(self,p):
        if(p is None):
            self._print_err("End of file reached")
        else:
            self._print_err("Syntax error at token %s" % p.type)
        
        
if __name__ == "__main__":
    p = QCParser()
    y = p.parse("""
        dimension m
        dimensions m n
        variable z
        parameter b
        dimensions m
        variable x(m,n)
        
        82 + 6/3
        dimension p""")
    
    print y
    print p._dimensions
    print p._variables
    print p._parameters