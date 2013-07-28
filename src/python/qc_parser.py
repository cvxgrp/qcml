from qc_ply import yacc
from qc_lexer import QCLexer
from expressions.expression import Number, Parameter, Variable, Sum
from expressions.qc_ast import Objective, Program
from properties.sign import Neither, Positive, Negative
from properties.shape import Scalar, Vector, Matrix, Shape, isscalar
# from qc_ast import Number, Parameter, Variable, \
#     Add, Mul, Transpose, \
#     Objective, RelOp, Program, \
#     ToVector, ToMatrix, Atom, Sum, Norm, Abs, \
#     Neither, Positive, Negative, Node, \
#     Shape, Scalar, Vector, Matrix, isscalar, isnumber

# TODO: dimlist, arraylist, and idlist are all very similar
# i would like to merge them.

# our own exception class
class QCError(Exception): pass

def _find_column(data,pos):
    last_cr = data.rfind('\n',0,pos)
    if last_cr < 0:
      last_cr = 0
    column = (pos - last_cr) + 1
    return column

class QCParser(object):
    """ QCParser parses QCML but does not perform rewriting.

        After parsing, the resulting program is rewritten using our
        rewriting rules.

        To perform code generation, we walk the rewritten tree
    """

    # operator precedence
    precedence = (
        ('left', 'PLUS', 'MINUS'),
        ('left', 'TIMES', 'DIVIDE'),
        ('right', 'UMINUS'),
        ('left', 'TRANSPOSE')
    )

    def __init__(self):
        self.lex = QCLexer();
        self.lex.build();
        self.tokens = self.lex.tokens
        self.parser = yacc.yacc(module = self)

    def parse(self, text):
        """ Parses QCML and returns an AST.

            text:
                A string containing QCML source

            XXX / note to self: the AST is traversed afterwards to be
            rewritten. a problem is just a collection of ASTs
        """

        # always append a newline to the end
        text += '\n'

        # try:
        return self.parser.parse(text, debug=False)
        # except QCError:
        #     pass
        # except Exception as e:
        #     self._print_err(e, False)

    def _print_err(self, token, msg, raise_error=True):
        """ Prints a QCML parse error.

            msg:
                A string containing the message we want to print.

            offset:
                An integer for the line offset
        """
        # get the entire string we just tried to parse
        data = self.lex.lexer.lexdata
        s = data.split('\n')

        if token is not None:
            num = token.lineno
            col = _find_column(data,token.lexpos)
        else:
            num = self.lex.lexer.lineno
            col = _find_column(data,self.lex.lexer.lexpos)
        line = s[num - 1]

        leader = 2*' '
        print "QCML error on line %s:" % num
        print leader, """>> %s """ % line.lstrip().rstrip()
        print leader, "  ", (col-1)*" ", "^"
        print
        print "Error:", msg
        print

        if raise_error:
            raise QCError(msg)

    def _name_exists(self,s):
        return (s in self.lex.dimensions) or \
               (s in self.lex.variables.keys()) or \
               (s in self.lex.parameters.keys())

    # only a single objective allowed per program
    def p_program(self,p):
        '''program : statements objective statements
                   | statements objective'''
        constraints = p[1]
        if len(p) > 3: constraints.extend(p[3])
        p[0] = Program(p[2], constraints, self.lex.variables, self.lex.parameters, self.lex.dimensions)

    def p_program_find(self,p):
        'program : statements'
        p[0] = Program(Objective('find', Number(0)), p[1], self.lex.variables, self.lex.parameters, self.lex.dimensions)

    def p_program_empty(self,p):
        'program : empty'
        pass

    def p_statements_statement(self,p):
        'statements : statement NL'
        p[0] = p[1]

    def p_statements_many_statement(self,p):
        'statements : statements statement NL'
        p[0] = []
        if p[1] is not None: p[0].extend(p[1])
        if p[2] is not None: p[0].extend(p[2])

    def p_statement(self,p):
        """statement : create
                     | constraint
                     | empty
        """
        # create returns None
        # constraint returns a list of constraints
        if p[1] is not None: p[0] = p[1]
        else: p[0] = []

    def p_objective(self,p):
        '''objective : SENSE expression NL
                     | SENSE expression NL SUBJ TO NL'''
        p[0] = Objective(p[1],p[2])


    def p_create_dimension(self,p):
        'create : DIMENSION ID'
        if self._name_exists(p[2]):
            self._print_err(p[2],"name '%s' already exists in namespace" % p[2])
        else:
            self.lex.dimensions.add(p[2])

    def p_create_dimensions(self,p):
        'create : DIMENSIONS idlist'
        self.lex.dimensions.update(p[2])

    def p_create_identifier(self,p):
        """create : VARIABLE array
                  | PARAMETER array
        """
        (name, shape) = p[2]
        if(p[1] == 'variable'):
            self.lex.variables[name] = Variable(name, shape)
        if(p[1] == 'parameter'):
            self.lex.parameters[name] = Parameter(name, shape, Neither())

    def p_create_identifiers(self,p):
        """create : VARIABLES arraylist
                  | PARAMETERS arraylist
        """
        if(p[1] == 'variables'):
            self.lex.variables.update({name: Variable(name, shape) for (name,shape) in p[2]})
        if(p[1] == 'parameters'):
            self.lex.parameters.update({name: Parameter(name, shape, Neither()) for (name,shape) in p[2]})

    def p_create_signed_identifier(self,p):
        'create : PARAMETER array SIGN'
        (name, shape) = p[2]
        if p[3] == 'positive' or p[3] == 'nonnegative':
            self.lex.parameters[name] = Parameter(name, shape, Positive())
        else:
            self.lex.parameters[name] = Parameter(name, shape, Negative())

    def p_array_identifier(self,p):
        'array : ID LPAREN dimlist RPAREN'
        if self._name_exists(p[1]):
            self._print_err(p[1],"name '%s' already exists in namespace" % p[1])
        else:
            p[0] = (p[1], Shape(p[3]))

    def p_array_identifier_scalar(self, p):
        'array : ID'
        if self._name_exists(p[1]):
            self._print_err(p[1],"name '%s' already exists in namespace" % p[1])
        else:
            p[0] = (p[1],Scalar())

    # (for shape) id, id, id ...
    def p_dimlist_list(self,p):
        'dimlist : dimlist COMMA DIM_ID'
        p[0] = p[1] + [p[3]]

    def p_dimlist_list_int(self,p):
        'dimlist : dimlist COMMA INTEGER'
        p[0] = p[1] + [p[3]]

    def p_dimlist_id(self,p):
        'dimlist : DIM_ID'
        p[0] = [p[1]]

    def p_dimlist_constant(self,p):
        'dimlist : INTEGER'
        p[0] = [p[1]]

    # (for declaring multiple dimensions) id id id ...
    def p_idlist_list(self,p):
        '''idlist : idlist ID'''
        if self._name_exists(p[2]):
            self._print_err(p[2],"name '%s' already exists in namespace" % p[2])
        else:
            p[0] = p[1] + [p[2]]

    def p_idlist_id(self,p):
        'idlist : ID'
        if self._name_exists(p[1]):
            self._print_err(p[2],"name '%s' already exists in namespace" % p[1])
        else:
            p[0] = [p[1]]

    # for declaring multiple variables, parameters
    def p_arraylist_list(self,p):
        '''arraylist : arraylist array'''
        p[0] = p[1] + [p[2]]

    def p_arraylist_array(self,p):
        'arraylist : array'
        p[0] = [p[1]]

    def p_constraint(self,p):
        '''constraint : expression EQ expression
                      | expression LEQ expression
                      | expression GEQ expression'''
        if p[2] == '==':
            p[0] = [p[1] == p[3]]
        elif p[2] == '<=':
            p[0] = [p[1] <= p[3]]
        else:
            p[0] = [p[1] >= p[3]]

    # more generic chained constraint is
    #    constraint EQ expression
    #    constraint LEQ expression
    #    constraint GEQ expression
    # not sure if we need to handle that
    def p_chained_constraint(self,p):
        '''constraint : expression LEQ expression LEQ expression
                      | expression GEQ expression GEQ expression'''
        if p[2] == '<=':
            p[0] = [ p[1] <= p[3], p[3] <= p[5] ]
        else:
            p[0] = [ p[1] >= p[3], p[3] >= p[5] ]


    def p_expression_add(self,p):
        'expression : expression PLUS expression'
        p[0] = p[1] + p[3] # expression + epxression

    def p_expression_minus(self,p):
        'expression : expression MINUS expression'
        p[0] = p[1] - p[3]

    def p_expression_divide(self,p):
        '''expression : expression DIVIDE CONSTANT
                      | expression DIVIDE INTEGER'''
        p[0] = Number(1.0/p[3]) * p[1]

    def p_expression_multiply(self,p):
        'expression : expression TIMES expression'
        p[0] = p[1] * p[3]

    def p_expression_group(self,p):
        'expression : LPAREN expression RPAREN'
        p[0] = p[2]

    def p_expression_negate(self,p):
        'expression : MINUS expression %prec UMINUS'
        p[0] = -p[2]

    def p_expression_transpose(self,p):
        'expression : expression TRANSPOSE'
        if isscalar(p[1]): p[0] = p[1]
        else: p[0] = Transpose(p[1])

    def p_expression_constant(self,p):
        """expression : CONSTANT
                      | INTEGER
                      | VAR_ID
                      | PARAM_ID"""
        # these are leaves in the expression tree
        if isinstance(p[1], float): p[0] = Number(p[1])
        elif isinstance(p[1], int): p[0] = Number(float(p[1]))
        elif isinstance(p[1], Variable): p[0] = p[1]
        elif isinstance(p[1], Parameter): p[0] = p[1]
        else: self._print_err(p[1], "Unknown identifier '%s'" % p[1])

    def p_expression_sum(self,p):
        'expression : SUM LPAREN expression RPAREN'
        if isscalar(p[3]): p[0] = p[3]
        else: p[0] = Sum(p[3])

    def p_expression_abs(self,p):
        'expression : ABS LPAREN expression RPAREN'
        p[0] = Abs(p[3])

    def p_expression_norm(self,p):
        'expression : NORM LPAREN arglist RPAREN'
        p[0] = Norm(p[3])

    def p_expression_atom(self,p):
        'expression : ATOM LPAREN arglist RPAREN'
        p[0] = Atom(p[1],p[3])

    def p_arglist(self, p):
        'arglist : arglist COMMA expression'
        p[0] = p[1] + [p[3]]

    def p_arglist_expr(self, p):
        'arglist : expression'
        p[0] = [p[1]]

    def p_empty(self,p):
        'empty : '
        pass

    # (Super ambiguous) error rule for syntax errors
    def p_error(self,p):
        if p is None: self._print_err(None, "End of file reached")
        else: self._print_err(p, "Syntax error at '%s'" % p.value)
