from ply import yacc

from . exceptions import ParseError
from . qc_lexer import QCLexer
from . ast.expressions import Number, Parameter, Variable, Sum, Transpose
from . ast.atoms import atoms
from . ast import SOCP, ProgramData, ProgramConstraints, ProgramObjective
from . properties.sign import Neither, Positive, Negative
from . properties.shape import Scalar, Shape, isscalar

# TODO: dimlist, arraylist, and idlist are all very similar
# i would like to merge them.

def _find_column(data, pos):
    last_cr = data.rfind('\n',0,pos)
    if last_cr < 0:
        last_cr = 0
    else:
        last_cr += 1 # since carriage return counts as a token
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
        self.lex = QCLexer()
        self.lex.build()
        self.tokens = self.lex.tokens
        self.parser = yacc.yacc(module = self)

        self.decl_parameters = {}
        self.decl_variables = {}
        self.decl_dimensions = set()
        self.decl_dual_variables = set()

        # while self.decl_parameters, self.decl_variables, and
        # self.decl_dimensions keep track of *declared* variables, parameters,
        # and dimensions, self.parameters, self.variables, and self.dimensions
        # keep track of *used* variables, parameters, and dimensions
        self.parameters = {}
        self.variables = {}
        self.dimensions = set()
        self.dual_variables = set()

        self.error_msg = None

    def parse(self, text):
        """ Parses QCML and returns an AST.

            text:
                A string containing QCML source

            XXX / note to self: the AST is traversed afterwards to be
            rewritten. a problem is just a collection of ASTs
        """

        # always append a newline to the end
        s = text.split('\n')
        text = '\n'.join(line.strip() for line in s)
        text += '\n'

        # try:
        return self.parser.parse(text, debug=False)
        # except QCError:
        #     pass
        # except Exception as e:
        #     self._show_err(e, False)

    def _show_err(self, msg, lineno, lexpos):
        """ Prints a QCML parse error.

            lineno:
                the line number of the error

            lexpos:
                the lexer position
        """
        # get the entire string we just tried to parse
        data = self.lex.lexer.lexdata
        s = data.split('\n')

        col = _find_column(data, lexpos)
        line = s[lineno-1]

        leader = 3*' '
        print "-"*72
        print "QCML error on line %s:" % lineno
        print leader, """>> %s """ % line.strip()
        print leader, "   " + (" "*(col-1)) + "^"
        print
        print "ERROR:", msg
        print "-"*72

    def _name_exists(self,s):
        return (s in self.decl_dimensions) or \
               (s in self.decl_variables.keys()) or \
               (s in self.decl_parameters.keys())

    def _check_if_defined(self, identifier, lineno, lexpos):
        if self._name_exists(identifier):
            msg = "name '%s' already exists in namespace" % identifier
            self._show_err(msg, lineno, lexpos)
            raise ParseError(msg)

    def _check_dimension(self, identifier, lineno, lexpos):
        if not isinstance(identifier, int):
            if identifier in self.decl_dimensions:
                self.dimensions.add(identifier)
            else:
                msg = "name '%s' does not name a valid dimension" % identifier
                self._show_err(msg, lineno, lexpos)
                raise ParseError(msg)

    # only a single objective allowed per program
    def p_program(self,p):
        '''program : statements objective statements
                   | statements objective
        '''
        constraints = p[1]
        if len(p) > 3: constraints.extend(p[3])
        constr = ProgramConstraints(constraints)
        data = ProgramData(self.dimensions, self.parameters, self.variables)
        p[0] = SOCP(p[2], constr, data)

    def p_program_find(self,p):
        'program : statements'
        obj = ProgramObjective('find', Number(0))
        constr = ProgramConstraints(p[1])
        data = ProgramData(self.dimensions, self.parameters, self.variables)
        p[0] = SOCP(obj, constr, data)

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
        '''statement : create
                     | constraint
                     | dual_constraint
                     | chained_constraint
                     | empty
        '''
        # create returns None
        # constraint returns a list of constraints
        if p[1] is not None: p[0] = p[1]
        else: p[0] = []

    def p_objective(self,p):
        '''objective : SENSE expression NL
                     | SENSE expression NL SUBJ TO NL'''
        p[0] = ProgramObjective(p[1],p[2])


    def p_create_dimension(self,p):
        'create : DIMENSION ID'
        self._check_if_defined(p[2], p.lineno(2), p.lexpos(2))
        self.decl_dimensions.add(p[2])

    def p_create_dimensions(self,p):
        'create : DIMENSIONS idlist'
        self.decl_dimensions.update(p[2])

    def p_create_identifier(self,p):
        '''create : VARIABLE array
                  | PARAMETER array
        '''
        (name, shape) = p[2]
        if(p[1] == 'variable'):
            self.decl_variables[name] = Variable(name, shape)
        if(p[1] == 'parameter'):
            self.decl_parameters[name] = Parameter(name, shape, Neither())

    def p_create_identifiers(self,p):
        '''create : VARIABLES arraylist
                  | PARAMETERS arraylist
        '''
        if(p[1] == 'variables'):
            self.decl_variables.update({name: Variable(name, shape) for (name,shape) in p[2]})
        if(p[1] == 'parameters'):
            self.decl_parameters.update({name: Parameter(name, shape, Neither()) for (name,shape) in p[2]})

    def p_create_signed_identifier(self,p):
        'create : PARAMETER array SIGN'
        (name, shape) = p[2]
        if p[3] == 'positive' or p[3] == 'nonnegative':
            self.decl_parameters[name] = Parameter(name, shape, Positive())
        else:
            self.decl_parameters[name] = Parameter(name, shape, Negative())

    def p_create_dual_variable(self, p):
        'create : DUAL VARIABLE ID'
        self._check_if_defined(p[3], p.lineno(3), p.lexpos(3))
        self.decl_dual_variables.add(p[3])

    def p_create_dual_variables(self, p):
        'create : DUAL VARIABLES idlist'
        self.decl_dual_variables.update(p[3])

    def p_array_identifier(self,p):
        'array : ID LPAREN dimlist RPAREN'
        self._check_if_defined(p[1], p.lineno(1), p.lexpos(1))
        p[0] = (p[1], Shape(p[3]))

    def p_array_identifier_scalar(self, p):
        'array : ID'
        self._check_if_defined(p[1], p.lineno(1), p.lexpos(1))
        p[0] = (p[1],Scalar())

    # (for shape) id, id, id ...
    def p_dimlist_list(self,p):
        '''dimlist : dimlist COMMA ID
                   | dimlist COMMA INTEGER
        '''
        self._check_dimension(p[3], p.lineno(3), p.lexpos(3))
        p[0] = p[1] + [p[3]]

    def p_dimlist_singleton(self,p):
        '''dimlist : INTEGER
                   | ID
        '''
        self._check_dimension(p[1], p.lineno(1), p.lexpos(1))
        p[0] = [p[1]]

    # (for declaring multiple dimensions) id id id ...
    def p_idlist_list(self,p):
        '''idlist : idlist ID'''
        self._check_if_defined(p[2], p.lineno(2), p.lexpos(2))
        p[0] = p[1] + [p[2]]

    def p_idlist_id(self,p):
        'idlist : ID'
        self._check_if_defined(p[1], p.lineno(1), p.lexpos(1))
        p[0] = [p[1]]

    # for declaring multiple variables, parameters
    def p_arraylist_list(self,p):
        'arraylist : arraylist array'
        p[0] = p[1] + [p[2]]

    def p_arraylist_array(self,p):
        'arraylist : array'
        p[0] = [p[1]]

    def p_constraint(self,p):
        '''constraint : expression EQ expression
                      | expression LEQ expression
                      | expression GEQ expression
        '''
        if p[2] == '==':
            p[0] = [p[1] == p[3]]
        elif p[2] == '<=':
            p[0] = [p[1] <= p[3]]
        else:
            p[0] = [p[1] >= p[3]]

    def p_dual_constraint(self,p):
        'dual_constraint : ID COLON constraint'
        if p[1] in self.decl_dual_variables:
            self.dual_variables.add(p[1])
            # a constraint is a singleton list
            p[3][0].dual_var = p[1]
        p[0] = p[3]


    # more generic chained constraint is
    #    constraint EQ expression
    #    constraint LEQ expression
    #    constraint GEQ expression
    # not sure if we need to handle that
    def p_chained_constraint(self,p):
        '''chained_constraint : expression LEQ expression LEQ expression
                              | expression GEQ expression GEQ expression
        '''
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
        '''expression : CONSTANT
                      | INTEGER
                      | ID'''
        # these are leaves in the expression tree
        if isinstance(p[1], float):
            p[0] = Number(p[1])
        elif isinstance(p[1], int):
            p[0] = Number(float(p[1]))
        else:
            variable = self.decl_variables.get(p[1], None)
            parameter = self.decl_parameters.get(p[1], None)
            if not variable and not parameter:
                msg = "Unknown identifier '%s'" % p[1]
                self._show_err(msg, p.lineno(1), p.lexpos(1))
                raise ParseError(msg)
            elif variable and parameter:
                msg = "Unknown error: '%s' names *both* a variable and parameter" % p[1]
                self._show_err(msg, p.lineno(1), p.lexpos(1))
                raise ParseError(msg)
            elif variable and not parameter:
                p[0] = variable
                self.variables[p[1]] = variable
            elif parameter and not variable:
                p[0] = parameter
                self.parameters[p[1]] = parameter


    def p_expression_sum(self,p):
        'expression : SUM LPAREN expression RPAREN'
        if isscalar(p[3]): p[0] = p[3]
        else: p[0] = Sum(p[3])

    # def p_expression_abs(self,p):
    #     'expression : ABS LPAREN expression RPAREN'
    #     p[0] = Abs(p[3])
    #
    # def p_expression_norm(self,p):
    #     'expression : NORM LPAREN arglist RPAREN'
    #     p[0] = Norm(p[3])

    def p_expression_atom(self,p):
        'expression : ATOM LPAREN arglist RPAREN'
        p[0] = atoms[p[1]](*p[3])

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
        if p:
            msg = "Syntax error at '%s'" % p.value
            self._show_err(msg,
                p.lexer.lineno,
                p.lexpos
            )

        else:
            msg = "End of file reached; missing newline at end of file?"
            self._show_err(msg,
                self.lex.lexer.lineno,
                self.lex.lexer.lexpos)
        raise ParseError(msg)
