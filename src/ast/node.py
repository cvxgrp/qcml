""" These are the basic node objects.

    Need to think about them in the context of a tree. An expression will now
    consist of three lists:
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
        We return its constraints
    
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
        
    But what happens when what *was* a single term becomes a linear expression?
"""
from abc import ABCMeta, abstractmethod
import sys


class Node(object):
    """ Every node in problem's AST has a show method which will display the
        subtree.
        
        Every node must implement three methods:
            info -- a description of the node
            children -- a generator that yields the node's children
            canonicalize -- a method that returns the canonicalized node
    """
    __metaclass__ = ABCMeta

    def show(self, buf = sys.stdout, offset = 0):
        """ Defines what to show; usually via buf.write
        """
        buf.write("%s%s\n" % (offset*' ', self.info()))
        for child in self.children():
            assert(isinstance(child, Node))
            child.show(buf, offset + 2)
    
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
  