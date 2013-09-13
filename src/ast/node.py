from abc import ABCMeta, abstractmethod
from qcml.mixins import Treelike, Showable, Canonicalizable
import sys

class Node(Treelike, Showable, Canonicalizable):
    __metaclass__ = ABCMeta
    """ Every node in problem's AST is treelike, showable, and
        canonicalizable.
    """

    @abstractmethod
    def info(self):
        """ Returns information about the node as a string.
        """
        pass

    def show(self, buf = sys.stdout, offset = 0):
        """ Defines what to show; usually via buf.write
        """
        buf.write("%s%s\n" % (offset*' ', self.info()))
        for child in self.children():
            assert(isinstance(child, Showable))
            child.show(buf, offset + 2)
