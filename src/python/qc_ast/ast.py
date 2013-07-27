import sys

""" Actual AST class
"""

class Node(object):
    """ Abstract base class for AST nodes.
    """
    def children(self):
        """ A sequence of all children that are Nodes
        """
        pass

    attr_names = () # attribute names to show

    def show(self, buf=sys.stdout, offset=0, attrnames=False, nodenames=False, _my_node_name=None):
        """ Pretty print the Node and all its attributes and
            children (recursively) to a buffer.

            buf:
                Open IO buffer into which the Node is printed.

            offset:
                Initial offset (amount of leading spaces)

            attrnames:
                True if you want to see the attribute names in
                name=value pairs. False to only see the values.

            nodenames:
                True if you want to see the actual node names
                within their parents.

            showcoord:
                Do you want the coordinates of each Node to be
                displayed.
        """
        lead = ' ' * offset
        if nodenames and _my_node_name is not None:
            buf.write(lead + self.__class__.__name__+ ' <' + _my_node_name + '>: ')
        else:
            buf.write(lead + self.__class__.__name__+ ': ')

        if self.attr_names:
            if attrnames:
                nvlist = [(n, getattr(self,n)) for n in self.attr_names]
                attrstr = ', '.join('%s=%s' % nv for nv in nvlist)
            else:
                vlist = [getattr(self, n) for n in self.attr_names]
                attrstr = ', '.join('%s' % v for v in vlist)
            buf.write(attrstr)

        # if showcoord:
        #     buf.write(' (at %s)' % self.coord)
        buf.write('\n')

        for (child_name, child) in self.children():
            if isinstance(child, list):
                for c in child:
                    c.show(
                        buf,
                        offset=offset + 2,
                        attrnames=attrnames,
                        nodenames=nodenames,
                        _my_node_name=child_name)
            else:
                child.show(
                    buf,
                    offset=offset + 2,
                    attrnames=attrnames,
                    nodenames=nodenames,
                    _my_node_name=child_name)

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
        """ Visit a node.
        """
        method = 'visit_' + node.__class__.__name__
        visitor = getattr(self, method, self.generic_visit)
        return visitor(node)

    def generic_visit(self, node):
        """ Called if no explicit visitor function exists for a
            node. Implements preorder visiting of the node.
        """
        for c_name, c in node.children():
            if isinstance(c, list):
                for elem in c:
                    if isinstance(elem, Node):
                        self.visit(elem)
            elif isinstance(c, Node):
                self.visit(c)

class NodeTransformer(NodeVisitor):
    """ A base NodeTransformer class for visiting c_ast nodes.
        Subclass it and define your own visit_XXX methods, where
        XXX is the class name you want to visit with these
        methods.

        For example:

        class ConstantVisitor(NodeTransformer):
            def __init__(self):
                self.values = []

            def visit_Constant(self, node):
                self.values.append(node.value)

        Creates a list of values of all the constant nodes
        encountered below the given node. To use it:

        cv = ConstantVisitor()
        cv.visit(node)

        Notes:

        *   generic_visit() will be called for AST nodes for which
            no visit_XXX method was defined.
        *   The children of nodes for which a visit_XXX was
            defined will not be visited - if you need this, call
            generic_visit() on the node.
            You can use:
                NodeTransformer.generic_visit(self, node)
        *   Modeled after Python's own AST visiting facilities
            (the ast module of Python 3.0)
    """
    def generic_visit(self, node):
        """ Called if no explicit visitor function exists for a
            node. Implements preorder visiting of the node.
        """
        for c_name, c in node.children():
            if isinstance(c, list):
                new_values = []
                for elem in c:
                    if isinstance(elem, Node):
                        elem = self.visit(elem)
                        if elem is None:
                            continue
                        elif not isinstance(elem, Node):
                            # returns a list
                            new_values.extend(elem)
                            continue
                    new_values.append(elem)
                c[:] = new_values
            elif isinstance(c, Node):
                new_node = self.visit(c)
                if new_node is None:
                    delattr(node, c_name)
                else:
                    setattr(node, c_name, new_node)
        return node

