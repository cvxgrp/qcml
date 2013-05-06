from scoop.qc_ast import NodeTransformer, Node

class NodeVisitor(object):
    """
    A node visitor base class that walks the abstract syntax tree and calls a
    visitor function for every node found.  This function may return a value
    which is forwarded by the `visit` method.

    This class is meant to be subclassed, with the subclass adding visitor
    methods.

    Per default the visitor functions for the nodes are ``'visit_'`` +
    class name of the node.  So a `TryFinally` node visit function would
    be `visit_TryFinally`.  This behavior can be changed by overriding
    the `visit` method.  If no visitor function exists for a node
    (return value `None`) the `generic_visit` visitor is used instead.

    Don't use the `NodeVisitor` if you want to apply changes to nodes during
    traversing.  For this a special visitor exists (`NodeTransformer`) that
    allows modifications.
    """

    def visit(self, node):
        """Visit a node."""
        method = 'visit_' + node.__class__.__name__
        visitor = getattr(self, method, self.generic_visit)
        return visitor(node)

    def generic_visit(self, node):
        """Called if no explicit visitor function exists for a node."""
        for field, value in node.children():
            if isinstance(value, list):
                for item in value:
                    if isinstance(item, Node):
                        self.visit(item)
            elif isinstance(value, Node):
                self.visit(value)

# TODO: remove above. copied from python AST

class CVXOPTCodegen(NodeVisitor):
    def __init__(self):
        self.lineq = {} # linear equation, keyed with varname
        self.expr_stack = []
        
    def visit_Variable(self, node):
        if node.value in self.varnames:
            lineq = {'_' + node.value: None}
        else:
            lineq= {node.value: None}
        self.expr_stack.append(lineq)
    
    def visit_Parameter(self, node):
        self.expr_stack.append({'1':node.value})
    
    def visit_Constant(self, node):
        self.expr_stack.append({'1':str(node.value)})
        
    def visit_Transpose(self, node):
        self.generic_visit(node)
        
        arg = self.expr_stack.pop()
        
        for k,v in arg.iteritems():
            if arg[k] is not None:
                arg[k] = arg[k] + ".trans()"
        
        self.expr_stack.append(arg)
    
    def visit_Mul(self, node):
        # at this stage, lineq stack guaranteed to contain a constant / parameter
        self.generic_visit(node)
        
        right = self.expr_stack.pop()
        left = self.expr_stack.pop()
        
        # left should always be known constant
        coeff = left['1']
        
        for k in right.keys():
            if right[k] is not None:
                # does not optimize gamma*matrix(1, (1,n))
                right[k] =  "%s * %s" % (coeff, right[k])
            else:
                right[k] = coeff
        
        self.expr_stack.append(right)
    
    def visit_Sum(self, node):
        self.generic_visit(node)
        
        arg = self.expr_stack.pop()
        
        for k in arg.keys():
            if arg[k] is not None:
                arg[k] =  "matrix(1,(1,%s)) * (%s)" % (node.shape.row, arg[k])
            else:
                arg[k] =  "matrix(1,(1,%s))" % (node.arg.shape.row)
        
        self.expr_stack.append(arg)
    
    def visit_Add(self, node):
        self.generic_visit(node)
        
        right = self.expr_stack.pop()
        left = self.expr_stack.pop()
        
        for k in right.keys():
            if left.get(k, None) is not None:
                left[k] = "%s + %s" % (left[k], right[k])
            else:
                left[k] = right[k]
        
        self.expr_stack.append(left)
        
    
    def visit_Objective(self, node):
        self.generic_visit(node)
        e = self.expr_stack.pop()
        print e
    
    def visit_RelOp(self, node):
        # TODO: in rewriter, force relop to be a - b <= 0
        # and a - b == 0
        self.generic_visit(node)
        right = self.expr_stack.pop()
        left = self.expr_stack.pop()
        
        for k in right.keys():
            if left.get(k, None) is not None:
                left[k] = "%s - (%s)" % (left[k], right[k])
            else:
                left[k] = right[k]
                
        print left
    
    
    def visit_Program(self, node):
        self.varnames = set(['_' + v for v in node.variables.keys()])
        self.new_varnames = set(node.new_variables.keys())
        
        self.generic_visit(node)
        
        print self.lineq