from qc_ast import NodeVisitor
from qc_atoms import atoms
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
class QCRewriter(NodeVisitor):
    varcount = 0
    
    def visit_Atom(self, node):
        p = atoms[node.name].rewrite(node,*node.arglist)
        print p
        node = None