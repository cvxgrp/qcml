from scoop.qc_ast import NodeVisitor, Negate
from scoop.qc_rewrite import QCRewriter

class CVXCodegen(NodeVisitor):
    offset = 2*' '
    constraint = 4*' '
     # the lookup is for adding comments
     # we can actually pass the lookup from the rewriter to the code generator
     # however, i am not doing that at the moment
    def __init__(self, comments):
        self.mappings = {v:k for k, v in comments.items()}
    
    def visit_Program(self, node):
        print "cvx_begin"      
        for v in node.variables.values():
            print self.offset, "variable %s(%s)" % (v, ', '.join(map(str, v.shape.dimensions)))
        print
        
        for v in node.new_variables.values():
            print self.offset, "variable %s(%s) %% = %s" \
                % (v, ', '.join(map(str, v.shape.dimensions)), self.mappings[v])
        print
        
        self.generic_visit(node)
        print "cvx_end"

    def visit_Objective(self, node):
        print self.offset, "%s (%s)" % (node.sense, node.expr)
        print self.offset, "subject to"
    
    def visit_RelOp(self, node):
        if node.op == '<=':
            print self.constraint, "%s + %s <= 0" % (node.left, Negate(node.right))
        elif node.op == '>=':
            print self.constraint, "%s + %s <= 0" % (node.right, Negate(node.left))
        else:
            print self.constraint, "%s + %s == 0" % (node.left, Negate(node.right))
    
    def visit_SOC(self, node):
        print self.constraint, node
    
    def visit_SOCProd(self, node):
        if len(node.arglist) == 1:
            print self.constraint, "abs(%s) <= %s" % (node.arglist[0], node.right)
        else:
            print self.constraint, "norms([%s])' <= %s" % ('; '.join(map(lambda x: "(%s)'" % x, node.arglist)), node.right)

