from abc import ABCMeta, abstractmethod

class Treelike(object):
    __metaclass__ = ABCMeta

    @abstractmethod
    def children(self):
        """ Defines a generator that yields the children of the tree
        """
        pass


