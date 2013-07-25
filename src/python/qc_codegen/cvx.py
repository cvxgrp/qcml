from qcml.qc_ast import NodeVisitor, Negate
from qcml.qc_rewrite import QCRewriter

class CVXCodegen(NodeVisitor):
    offset = 2*' '
    constraint = 4*' '
     # the lookup is for adding comments
     # we can actually pass the lookup from the rewriter to the code generator
     # however, i am not doing that at the moment

    def __init__(self, dims = None):
        # second argument is ignored
        self.prog = []

    # TODO: this ought to be rolled into a master codegen class that
    # CVXCodegen can subclass....
    def codegen(self):
        def not_implemented():
            raise Exception("Code generator not implemented for %s" % (self.__class__.__name__))
        return not_implemented

    def prettyprint(self,lineno=False):
        """ Pretty prints the source code, possibly with line numbers
        """
        if lineno:
            print '\n'.join( map(lambda x: "%4s    %s" % (x[0],x[1]), zip( range(1,len(self.prog)+1), self.prog ))  )
        else:
            print '\n'.join(self.prog)

    def visit_Program(self, node):
        self.prog.append("cvx_begin")
        for v in node.variables.values():
            self.prog.append(self.offset + "variable %s(%s)" % (v, ', '.join(map(str, v.shape.dimensions))))
        self.prog.append("")

        for v in node.new_variables.values():
            self.prog.append(self.offset + "variable %s(%s)" % (v, ', '.join(map(str, v.shape.dimensions))))
        self.prog.append("")

        self.generic_visit(node)
        self.prog.append("cvx_end")

    def visit_Objective(self, node):
        self.prog.append(self.offset + "%s (%s)" % (node.sense, node.expr))
        self.prog.append(self.offset + "subject to")

    def visit_RelOp(self, node):
        if node.op == '<=':
            self.prog.append(self.constraint + "%s <= 0" % (node.left))
        elif node.op == '>=':
            self.prog.append(self.constraint + "%s <= 0" % (node.right))
        else:
            self.prog.append(self.constraint + "%s == 0" % (node.left))

    def visit_SOC(self, node):
        self.prog.append(self.constraint + str(node))

    def visit_SOCProd(self, node):
        if len(node.arglist) == 1:
            self.prog.append(self.constraint + "abs(%s) <= %s" % (node.arglist[0], node.right))
        else:
            self.prog.append(self.constraint + "norms([%s])' <= %s" % ('; '.join(map(lambda x: "(%s)'" % x, node.arglist)), node.right))

