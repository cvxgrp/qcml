""" This is the node interface.

    A node is...

------------------------------------------------------------------------------

    Need to think about them in the context of an n-ary tree. An expression
    will now consist of three (homogeneous) lists:
        linear terms
        nonlinear terms
        constants

    It represents an implicit addition.

    All terms are represented as
        a constant
        a list of parameters
        a variable (or a nonlinear function)

    It represents an implicit multiplication.

    A: So when we "canonicalize" an expression what happens?

        We *only* visit the nonlinear terms.
        We pop off each nonlinear term and
            -- canonicalize its children
            -- canonicalize its definition
        We then get its "objective" and its "constraints"
            -- these are going to be *other* (linear) expressions
            -- we push these onto the current expression's linear term list
        We return the constraints?

    Canonicalizing an objective:
        We canonicalize its child
        We return its constraints--no, make this a property.

    Canonicalizing a constraint:
        We canonicalize its children
        We return any additional constraints

    Canonicalizing a problem:
        We canonicalize its children
        We append any additional constraints

    Canonicalizing a term:
        If variable is a Variable, do nothing
        If it is a nonlinear function, we do (A) above.

    This means a "term" can be asked for its objective and its constraint. In
    fact, any node can be asked for its objective and its constraints.

    Actually, when parsing, an atom can already construct its objective
    and its constraints from its arguments. It doesn't need to do anything
    with it yet.

    ....?

    So "Variables" + "Atoms" are the same "kind" of objects while everything
    else is, well, something else. They're all expression nodes (of course),
    but Variables and Atoms can be asked for their objectives and constraints.

    Actually, what gets canonicalized is a *term*.

    So only terms can be canonicalized.... okay...

    So for an expression...

    Canonicalizing an expression:
        We canonicalize its children
        We return its constraints (constructed from its children)

    But what happens when what *was* a single term becomes a linear
    expression?

    hmm...  can you ask a Program to canonicalize itself? yes. can you ask
    a constraint, an objective, an expr, an atom, a term, etc. to canonicalize
    itself? yes. yes. and yes.

    but "canonicalize" shouldn't return anything. if you canonicalize it,
    you're done. you should, however, after canonicalizing, be able to access
    any new constraints and new variables introduced.... in other words, it
    ought to have some side effect.

    altneratively, i can "go the way" of Variable, where the counter is part
    of the Variable class; i can make "constraints" part of the Program
    class....

    This works with Variables because I don't "nest" Variables; will I "nest"
    Programs? I guess that's only minorly useful.
"""
from abc import ABCMeta, abstractmethod
import sys


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

    def show(self, buf = sys.stdout, offset = 0):
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



    # @abstractmethod
    # def constraints(self):
    #     """ Defines a generator that yields the constraints of the tree
    #     """
    #     pass

