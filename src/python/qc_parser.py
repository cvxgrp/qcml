from qc_ply import yacc
from qc_lex import QCLexer
from qc_ast import Constant, Parameter, Variable, Add, Negate, Mul, Objective, RelOp, Program
from qc_sign import Neither, Positive, Negative
from utils import isconstant, isadd, ismul, isparameter
import operator

def negate_node(x):
    """ Negates an AST node"""
    if isconstant(x):
        return Constant(-x.value)
    elif isadd(x):
        if isconstant(x.left):
            return Add(Constant(-x.left.value), Negate(x.right))
        else:
            return Add(Negate(x.left), Negate(x.right))
    elif ismul(x):
        if isconstant(x.left):
            return Mul(Constant(-x.left.value), x.right)
        else:
            return Mul(Negate(x.left), x.right)
    else:
        return Negate(x)

def constant_folding(lhs,rhs,op,isop,do_op):
    if isconstant(lhs) and isconstant(rhs):
        return Constant(do_op(lhs.value, rhs.value))
    
    left = lhs
    right = rhs
    if isconstant(lhs) and isop(rhs):
        # left is constant and right is the result of an add
        # by convention, we'll put constants on the left leaf
        if isconstant(rhs.left):
            right = rhs.right
            left = Constant(do_op(rhs.left.value,lhs.value))
        else:
            right = rhs
            left = lhs
    elif isconstant(rhs) and isop(lhs):
        # right is constant and left is the result of an add
        # by convention, we'll put constants on the right leaf
        if isconstant(lhs.left):
            right = lhs.right
            left = Constant(do_op(lhs.left.value,rhs.value))
    elif isop(lhs) and isop(rhs) and isconstant(lhs.left) and isconstant(rhs.left):
        # if adding two add nodes with constants on both sides
        left = Constant(do_op(lhs.left.value, rhs.left.value))
        right = op(lhs.right, rhs.right)
    elif isop(lhs) and isconstant(lhs.left):
        # if there are constants on the lhs, move up tree
        left = lhs.left
        right = op(lhs.right, rhs)
    elif isop(rhs) and isconstant(rhs.left):
        # if there are constants on the rhs, move up tree    
        left = rhs.left
        right = op(lhs,rhs.right)  
    
    return op(left, right)
        
def constant_folding_add(lhs,rhs):
    return constant_folding(lhs, rhs, Add, isadd, operator.add)

def constant_folding_mul(lhs,rhs):
    return constant_folding(lhs, rhs, Mul, ismul, operator.mul)
    
def distribute(lhs, rhs):
    if isadd(lhs):
        return constant_folding_add(
            distribute(lhs.left, rhs), 
            distribute(lhs.right, rhs)
        )
    elif isadd(rhs):
        return constant_folding_add(
            distribute(lhs,rhs.left), 
            distribute(lhs,rhs.right)
        )
    else: 
        return constant_folding_mul(lhs,rhs)
        
class QCParser(object):
    """ QCParser parses QCML but does not check DCP compliance.
    """
    def __init__(self):
        self.lex = QCLexer();
        self.lex.build();
        self.tokens = self.lex.tokens
        self.parser = yacc.yacc(module = self)
        self.has_error = False
        
        self._dimensions = set()
        self._variables = {}
        self._parameters = {}
        
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
        result = self.parser.parse(text, debug=False)
        if self.has_error:
            # TODO: reset stuff
            return None
        else:
            return result
    
    def _print_err(self, msg, offset=1):
        """ Prints a QCML parse error.
        
            msg:
                A string containing the message we want to print.
            
            offset:
                An integer for the line offset
        """
        if not self.has_error:
            self.has_error = True
            s = self.lex.lexer.lexdata.split('\n')
            num = self.lex.lexer.lineno - offset
            print ">> line %s: %s" % (num+1, s[num].lstrip().rstrip())
            print msg
        raise SyntaxError
    
    def _name_exists(self,s):
        return (s in self._variables.keys() or s in self._parameters.keys() or s in self._dimensions)
        
    precedence = (
        ('left', 'PLUS', 'MINUS'),
        ('left', 'TIMES', 'DIVIDE'),
        ('right', 'UMINUS')
    )
    
    def p_program(self,p):
        """program : lines objective lines
                   | empty"""
        if p[3] is not None:
            p[1] += p[3]
        p[0] = Program(p[2], p[1])
    
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
    
    def p_objective(self,p):
        '''objective : SENSE expression NL
                     | SENSE expression NL subject_to NL'''
        p[0] = Objective(p[1],p[2])
    
    def p_subject_to(self,p):
        'subject_to : SUBJ TO'
        pass
        
    
    def p_declaration(self,p):
        """declaration : create 
                       | constraint
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
        if self._name_exists(p[2]):
            self._print_err("name '%s' already exists in namespace" % p[2])
        else:
            if(p[1] == 'variable'):
                self._variables[p[2]] = Variable(p[2])
            if(p[1] == 'parameter'):
                self._parameters[p[2]] = Parameter(p[2], Neither())
            if(p[1] == 'dimension'):
                self._dimensions.add(p[2])
    
    def p_create_signed_identifier(self,p):
        """create : PARAMETER array SIGN
                  | PARAMETER ID SIGN"""
        if self._name_exists(p[2]):
            self._print_err("name '%s' already exists in namespace" % p[2])
        else:
            if p[3] == 'positive' or p[3] == 'nonnegative':
                self._parameters[p[2]] = Parameter(p[2], Positive())
            else:
                self._parameters[p[2]] = Parameter(p[2], Negative())
    
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
        if self._name_exists(p[2]):
            self._print_err("name '%s' already exists in namespace" % p[2])
        else:
            p[0] = p[1] + [p[2]]
    
    def p_idlist_id(self,p):
        'idlist : ID'
        if self._name_exists(p[1]):
            self._print_err("name '%s' already exists in namespace" % p[1],2)
        else:
            p[0] = [p[1]]
    
    def p_constraint(self,p):
        '''constraint : expression EQ expression
                      | expression LEQ expression
                      | expression GEQ expression'''
        p[0] = RelOp(p[2],p[1],p[3])
    
    
    def p_expression_add(self,p):
        'expression : expression PLUS expression'
        # OK: performs constant folding
        # does not reduce x + x into 2*x
        p[0] = constant_folding_add(p[1],p[3])
    
    def p_expression_minus(self,p):
        'expression : expression MINUS expression'
        # OK: performs constant folding
        # does not reduce x - x into 0
        p[0] = constant_folding_add(p[1], negate_node(p[3]))
    
    def p_expression_divide(self,p):
        'expression : expression DIVIDE expression'
        if isconstant(p[1]) and isconstant(p[3]):
            p[0] = Constant(p[1].value / p[3].value)
        else:
            self._print_err("cannot divide non-constants %s and %s" % (p[1],p[3]))
            
    def p_expression_multiply(self,p):
        'expression : expression TIMES expression'
        p[0] = distribute(p[1],p[3])
    
    def p_expression_group(self,p):
        'expression : LPAREN expression RPAREN'
        p[0] = p[2]
    
    def p_expression_constant(self,p):
        """expression : CONSTANT
                      | ID"""
        if isinstance(p[1], float):
            p[0] = Constant(p[1])
        elif p[1] in self._variables.keys():
            p[0] = self._variables[p[1]]
        elif p[1] in self._parameters.keys():
            p[0] = self._parameters[p[1]]
        else:
            self._print_err("Unknown identifier %s" % p[1], 2)
    
    # (Super ambiguous) error rule for syntax errors
    def p_error(self,p):
        if(p is None):
            self._print_err("End of file reached")
        else:
            self._print_err("Syntax error at token %s" % p.type)
        
        
if __name__ == "__main__":
    p = QCParser()
    y = p.parse("""
        dimensions m n
        variable z
        parameter b
        parameter gamma positive
        variable x(m,n)
        
        82 + 6/3 == 1
        dimension p
        8 + gamma <= 5
        8+x + z - b+1 == 3
        8+x-1+z >= 1
        (21 + x)*(21 + x) == 4
        3*(21+x)-2*x*5+8-(4+1) == 4
        (b+b)*z + x*z <= x
        (b*z - 2) - (b*z - 2) >= z
        (b + b + b)*(x + z + b) == 0
        
        minimize x + b
        subject to
        """)
    
    y.show()
    print p._dimensions
    print p._variables
    print p._parameters