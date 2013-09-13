from abc import ABCMeta, abstractmethod

class Showable(object):
    __metaclass__ = ABCMeta

    @abstractmethod
    def show(self, buf, offset):
        """ Defines what to show; usually via buf.write
        """
        pass
