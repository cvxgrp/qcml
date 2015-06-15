""" This is the node interface.

    A node is an element of the problem tree.

    It is showable;

        node.show()

    will print the relevant information from the node and its subtree.

    It is canonicalizable;

        node.canonical_form()

    will convert the current node into a standard form SOCP.

    It is visitable;

        node.children()

    will produce a generator that iterates through the children of the node.

    Although I could separate these three function (show, canonical_form,
    and children) into three separate interface classes, it is better document
    them all here.
"""
from abc import ABCMeta, abstractmethod


class Node(object):
    """ A node lives in the context of an optimization *program*. It is able
        to reference the owning program and modify its variables, its problem,
        and so on.

        Every node in problem's AST has a show method which will display the
        subtree.

        Every node must implement three methods:
            info -- a description of the node
            children -- a generator that yields the node's children
            canonicalize -- a method that canonicalizes the expression
            constraints -- a generator that yields the constraints
    """
    __metaclass__ = ABCMeta
    # def __init__(self, parent_program, *args, **kwargs):
    #     self.__program = parent_program
    #     super(Node, self).__init__(*args, **kwargs)

    def show(self, buf, offset = 0):
        """ Defines what to show; usually via buf.write
        """
        buf.write("%s%s\n" % (offset*' ', self.info()))
        for child in self.children():
            assert(isinstance(child, Node))
            child.show(buf, offset + 2)

    # def canonicalize(self):
    #     """ Defines what happens when an object is canonicalized.
    #     """
    #     for child in self.children():
    #         assert(isinstance(child, Node))
    #         child.canonicalize()

    @abstractmethod
    def info(self):
        """ Returns information about the node as a string.
        """
        pass

    @abstractmethod
    def children(self):
        """ Defines a generator that yields the children of the tree
        """
        pass

    @abstractmethod
    def canonicalize(self):
        """ Defines what happens when an object is canonicalized.
        """
        pass

    #@abstractmethod
    def canonical_form(self):
        """ Converts the node into an SOCP and returns it.
        """
        pass

    # @abstractmethod
    # def constraints(self):
    #     """ Defines a generator that yields the constraints of the tree
    #     """
    #     pass
