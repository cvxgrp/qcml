from qc_ast import NodeTransformer, Constant, Variable, RelOp
from qc_atoms import atoms, norm_rewrite, abs_rewrite

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

# only rewrites the ATOM nodes
# TODO: problem with rewriter is that atoms defined like square(square(x)) aren't properly handled....
# i mean, their definition is handled fine, but the rewriting just inserts square(square(x)) unconditionally...
# i actually needed square(square(x)) inserted in the definition *before* i did the rewriting....
class QCRewriter(NodeTransformer):
    varcount = 0
    lookup = {}
    new_variables = {}
    
    def __init__(self):
        self.new_constraints = []
        self._new_variables = {}
        self.variables = {}
        self.parameters = {}
        self._existing_expression = {}
    
    def replaced_expressions(self):
        return self._existing_expression
    
    def _apply_rewrite_rule(self, node, f, *args):
        (v, constraints) = f(node,*args)
        if isinstance(v, Variable):
            QCRewriter.new_variables[v.value] = v
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
        # visit children first (in case they are rewritten)
        self.generic_visit(node)
        
        # now rewrite the current node
        v = self._existing_rewrite(node)
        
        if not v:
            f = atoms[node.name].rewrite
            return self._apply_rewrite_rule(node, f, *node.arglist)
        return v

    def visit_Norm(self, node):
        # visit / rewrite children first
        self.generic_visit(node)
        v = self._existing_rewrite(node)
        
        if not v:
            #if self.rewrite_norm:
            return self._apply_rewrite_rule(node, norm_rewrite, node.arglist)
            # each constraint can only have a single Norm
            # so although Norm(x) + Norm(y) <= z is a valid constraint, we
            #   can't turn it into an SOC unless one of them is rewritten
            # self.rewrite_norm = True
            # self.norm_node = node
            # return Constant(0)
        return v
    
    def visit_Abs(self, node):
        # visit / rewrite children first
        self.generic_visit(node)
        v = self._existing_rewrite(node)
        
        if not v:
            #if self.rewrite_norm:
            return self._apply_rewrite_rule(node, abs_rewrite, node.arglist)
            # each constraint can only have a single Abs
            # so although Abs(x) + Abs(y) <= z is a valid constraint, we
            #   can't turn it into an SOC unless one of them is rewritten
            # self.rewrite_norm = True
            # self.norm_node = node
            # return Constant(0)
        return v
        
    def visit_RelOp(self, node):
        # visit children
        self.generic_visit(node)
        
        # convert all constraints so they are 
        #  == 0 or <= 0
        if node.op == '==':
            return RelOp('==', node.left - node.right, Constant(0))
        if node.op == '<=':
            return RelOp('<=', node.left - node.right, Constant(0))
        if node.op == '>=':
            return RelOp('<=', node.right - node.left, Constant(0))
    
    def visit_Program(self, node):
        # load the current state
        QCRewriter.lookup = self._existing_expression
        QCRewriter.new_variables = self._new_variables
        
        # visit children first
        self.generic_visit(node)
        # now, update the variables and constraints
        node.new_variables = QCRewriter.new_variables
        node.constraints += filter(None, self.new_constraints)
        # remove any redundant constraints by converting to set
        node.constraints = list(set(node.constraints))
        # only include the variables and parameters that are used
        node.variables = self.variables
        node.parameters = self.parameters
        
        print node.variables
        print node.parameters
        print node.new_variables
        node.show()
        
        print QCRewriter.lookup
        
        # save state
        self._existing_expression = QCRewriter.lookup
        self._new_variables = QCRewriter.new_variables
        return node
#
# 5/1: stuff below is outdated now....
#

    # def visit_RelOp(self, node):
    #     # visit children
    #     self.generic_visit(node)
    #     
    #     if node.op == '==':
    #         return RelOp('<=')
            
    
    # def visit_Objective(self, node):
    #     self.rewrite_norm = True
    #     self.generic_visit(node)
    #     
    #     return node
    #     
    # def visit_RelOp(self, node):
    #     self.rewrite_norm = False
    #     self.norm_node = None
    #     self.generic_visit(node)
    #     
    #     return node
        
        
    # possible uses of Norm or Abs
    #   affine + norm(affine) <= affine
    # RelOp( left = Add, Right = Add )
    #
    # norm(affine) <= affine
    # RelOp(left = Norm, Right = Add)
    #
    # let's rewrite all RelOp's so they look like
    # norm(affine) <= affine
    # 0 <= affine
    # or
    # 0 == affine
    # affine >= norm(affine)
    # affine >= 0
    # etc.
    #
    # then, after rewriting, if "<=" and left is Norm / Abs, emit SOC
    # if ">=" and right is Norm / Abs, emit SOC
    #
    # only rewrite Norm when it appears in the objective