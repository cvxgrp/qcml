from .. import Node, NodeVisitor
from . objective import Objective
from .. constraints import LinearEquality, LinearInequality, SOC, SOCProd, Constraint
from .. expressions import Variable

""" Okay, so now, a Program will contain a problem instance.
    
    The "new variables" are part of this program
    The "new constraints" are a part of the problem.
    
    One possibility is to pass the program and problem object down to the
    children. That is, all nodes have access to the "root" object. I guess
    that is probably the way that makes the most sense.
    
    Or, better yet, there's
    
        program
                
               problem
                 /  \
              obj  constraints
               |       / | \
              expr   e1 e2 e3   
        
    And everyone is part of the program. So a "node" knows the program
    it lives in.
    
    You could parse(s1) then parse(s2) then canonicalize s1 and then
    canonicalize s2. Then codegen s1. When you cod
"""

class Program(Node):
    """ Program node.

        This node is root node for the program. Its children is a *single*
        Objective node and a list of RelOp nodes.

        It is DCP compliant when the Objective is DCP and the RelOp's are DCP.
    """
    def __init__(self, objective, constraints, variables, parameters={}, dimensions=set()):
        assert(isinstance(objective, Objective))
        assert(all(isinstance(x, Constraint) for x in constraints))

        self.objective = objective

        # adds constraints to the proper set (using constr_map) and removes
        # duplicates
        self.eq_constr, self.ineq_constr, self.soc_constr = set(), set(), set()
        self.__constr_map = {
            LinearEquality: self.eq_constr.add,
            LinearInequality: self.ineq_constr.add,
            SOC: self.soc_constr.add,
            SOCProd: self.soc_constr.add,
            None.__class__: lambda x: None,   # if None, don't do anything
        }
        for c in constraints:
            self.__constr_map[c.__class__](c)

        # TODO: Somehow, this stuff below is a different "thing"
        self.is_dcp = objective.is_dcp and all(c.is_dcp for c in constraints)
        self.__dimensions = dimensions # dimensions declared by the user
        self.variables = variables     # variables declared by the user
        self.parameters = parameters   # parameters declared by the user
        self.new_variables = {}        # new variables introduced by rewriting

    def __str__(self):
        constrs = [str(x) for x in self.eq_constr] + \
            [str(x) for x in self.ineq_constr] + \
            [str(x) for x in self.soc_constr]
        return "%s\nsubject to\n    %s" % (self.objective, '\n    '.join(constrs))

    def info(self):
        if self.is_dcp:
            return "DCP program:"
        else:
            return "Non-DCP program:"

    def canonicalize(self):
        Variable.reset()        # reset the variable state
        constraints = []
        for c in self.children():
            _, constr = c.canonicalize()
            constraints.extend(expr.simplify() for expr in constr)

        # clear the constraint sets
        self.eq_constr.clear()
        self.ineq_constr.clear()
        self.soc_constr.clear()
        for c in constraints:
            self.__constr_map[c.__class__](c)

        # doesn't actually need to return anything...
        self.new_variables = Variable.anonymous_variables

    def children(self):
        """ An iterator that yields the children
        """
        yield self.objective
        for c in self.eq_constr:
            yield c
        for c in self.ineq_constr:
            yield c
        for c in self.soc_constr:
            yield c

    def add_constraint(self, c):
        """ Allows us to add constraints to the program
        """
        self.__constr_map[c.__class__](c)

    @property
    def dimensions(self):
        return self.__dimensions

    @property
    def abstract_dims(self):
        return filter(lambda x: isinstance(x,str), self.dimensions)

    @dimensions.setter
    def dimensions(self, dims):
        (v.shape.eval(dims) for v in self.variables.values())
        (v.shape.eval(dims) for v in self.new_variables.values())
        (v.shape.eval(dims) for v in self.parameters.values())
        self.DimsSetter(dims).generic_visit(self)
        self.__dimensions = dims
        
    class DimsSetter(NodeVisitor):
        """ Goal is to traverse the tree and call eval(dims) on every 
            possible shape.
        """
        def __init__(self, dims):
            self.dims = dims

        def generic_visit(self, node):
            if hasattr(node, 'shape'): node.shape.eval(self.dims)
            super(Program.DimsSetter, self).generic_visit(node)
