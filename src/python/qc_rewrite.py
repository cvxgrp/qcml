# TODO: i should remove this file entirely....
import expressions.ast as ast
from constraints.soc import SOC, SOCProd, SOCConstraint
from constraints.linear import LinearConstraint
from atoms.atom import atoms
import atoms.qc_norm as norm

""" For rewriting atoms.

    Atoms need to have an AST node....

    class Atom(Node): pass

    They are "mini" programs with an objective and "sense".

    Their "leaves" are left floating?

    They're just inserted into the AST via the Visitor pattern...

    e.g.,

               +
           /       \
       atom(expr)  expr

    becomes

            +
        /       \
      expr     expr

    and other constraints, which are a function of the atom's arguments.

    The new expr on the lefthand side might contain new variables....

    so calling

        square :: Expression -> (Expression, [RelOp])

    builds the expression tree and the relative operators.

    this means an "Atom" AST Node contains the DCP properties
    (Increasing, Decreasing, Positive, Negative, Convex, Concave, etc.).
    finally, it contains a string so we can lookup the rewriting function.

    We also need to distinguish between an Atom that is rewritten and a
    builtin that is not rewritten (e.g., norm() <= affine and sum())

"""

# TODO: turn Atom nodes into classes / objects with a "canonicalize" function,
# a "size" function, etc. see Steven's CVXPY implementation, removes the need
# for all these special cases, although we do have to be careful with
# "repeated" expressions
#
# TODO: add a test case for repeated expressions

# only rewrites the ATOM nodes
# TODO: problem with rewriter is that atoms defined like square(square(x)) aren't properly handled....
# i mean, their definition is handled fine, but the rewriting just inserts square(square(x)) unconditionally...
# i actually needed square(square(x)) inserted in the definition *before* i did the rewriting....
class QCRewriter(ast.NodeTransformer):
    varcount = 0
    lookup = {}
    new_variables = {}

    def __init__(self):
        self.new_constraints = []
        self._new_variables = {}
        self.variables = {}
        self.parameters = {}
        self._existing_expression = {}

    def _apply_rewrite_rule(self, node, f, *args):
        (v, constraints) = f(node,*args)
        # if isinstance(v, Variable):
        #     QCRewriter.new_variables[v.value] = v
        self.new_constraints.extend(constraints)
        # store the variable as pointing to the expression
        QCRewriter.lookup[str(node)] = v
        return v

    def _existing_rewrite(self, node):
        return QCRewriter.lookup.get(str(node), None)

    def visit_Variable(self, node):
        self.variables[node.value] = node
        return node

    def visit_Parameter(self, node):
        self.parameters[node.value] = node
        return node

    def visit_Atom(self, node):
        if isinstance(node, norm.QC_norm):
            self.norm_node = node
        # visit children
        self.generic_visit(node)
        return node

        # # now rewrite the current node
        # v = self._existing_rewrite(node)
        #
        # if not v:
        #     f = atoms[node.name].rewrite
        #     return self._apply_rewrite_rule(node, f, *node.arglist)
        # return v

    # def visit_Norm(self, node):
    #     # visit / rewrite children first
    #     self.generic_visit(node)
    #     v = self._existing_rewrite(node)
    #
    #     if not v:
    #         if self.rewrite_norm:
    #             return self._apply_rewrite_rule(node, norm_rewrite, node.arglist)
    #         # each constraint can only have a single Norm
    #         # so although Norm(x) + Norm(y) <= z is a valid constraint, we
    #         #   can't turn it into an SOC unless one of them is rewritten
    #         self.rewrite_norm = True
    #         self.norm_node = node
    #         return Number(0)
    #     return v
    #
    # def visit_Abs(self, node):
    #     # visit / rewrite children first
    #     self.generic_visit(node)
    #     v = self._existing_rewrite(node)
    #
    #     if not v:
    #         if self.rewrite_norm:
    #             return self._apply_rewrite_rule(node, abs_rewrite, node.arg)
    #         # each constraint can only have a single Abs
    #         # so although Abs(x) + Abs(y) <= z is a valid constraint, we
    #         #   can't turn it into an SOC unless one of them is rewritten
    #         self.rewrite_norm = True
    #         self.norm_node = node
    #         return Number(0)
    #     return v

    # def visit_Add(self, node):
    #     self.generic_visit(node)
    #     return (node.left + node.right)
    #
    # def visit_Mul(self, node):
    #     self.generic_visit(node)
    #     return (node.left * node.right)
    #
    # def visit_Negate(self, node):
    #     self.generic_visit(node)
    #     return (-node.expr)

    # def visit_RelOp(self, node):
    #     # visit children
    #     self.generic_visit(node)
    #
    #     # convert all constraints so they are
    #     #  == 0 or <= 0
    #     if node.op == '==':
    #         return RelOp('==', node.left - node.right, Number(0))
    #     if node.op == '<=':
    #         return RelOp('<=', node.left - node.right, Number(0))
    #     if node.op == '>=':
    #         return RelOp('<=', node.right - node.left, Number(0))

    def visit_Program(self, node):
        # load the current state
        QCRewriter.lookup = self._existing_expression
        QCRewriter.new_variables = self._new_variables

        # visit children first, to gather all the used variables and parameters
        self.generic_visit(node)
        
        node.canonicalize()
        # now, update the variables and constraints
        node.new_variables = QCRewriter.new_variables

        #node.constraints += filter(None, self.new_constraints) # ALREADY DONE

        # remove any redundant constraints by converting to set
        unique_constraints = set(node.constraints)
        # convert to a list but place the linear constraints at the front of the list
        # could probably just have a separate "eq", "linineq", "soc" lists...
        linear_constraints = [x for x in unique_constraints if isinstance(x,LinearConstraint)]
        soc_constraints = [x for x in unique_constraints if isinstance(x,SOCConstraint)]
        node.constraints = linear_constraints + soc_constraints

        # only include the variables and parameters that are used
        node.variables = self.variables
        node.parameters = self.parameters


        # save state
        self._existing_expression = QCRewriter.lookup
        self._new_variables = QCRewriter.new_variables
        return node


    # def visit_Objective(self, node):
    #     self.rewrite_norm = True
    #     self.generic_visit(node)
    #
    #     return node

    def visit_RelOp(self, node):
        # self.rewrite_norm = False
        self.norm_node = None
        self.generic_visit(node)

        if self.norm_node is not None:
            if isinstance(self.norm_node, norm.QC_norm):
                if len(self.norm_node.args) == 1:
                    return SOC(-node.left, self.norm_node.args)
                else:
                    return SOCProd(-node.left, self.norm_node.args)
            else:   # abs
                return SOCProd(-node.left, [self.norm_node.args[0]])
        else:
            return node

    def visit_LinearEquality(self, node):
        return self.visit_RelOp(node)

    def visit_LinearInequality(self, node):
        return self.visit_RelOp(node)

