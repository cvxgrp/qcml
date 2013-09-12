from abc import ABCMeta, abstractmethod

class Canonicalizable(object):
    __metaclass__ = ABCMeta
    
    @abstractmethod
    def canonicalize(self):
        """ Defines what happens when an object is canonicalized.
        """
        pass