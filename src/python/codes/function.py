import collections
from itertools import chain
from abc import ABCMeta, abstractmethod

def iterable_line(line):
    if isinstance(line, str):
        return (l for l in [line])
    elif isinstance(line, collections.Iterable):
        return line
    else:
        raise TypeError("Expected a string or an iterable argument.")

class Function(object):
    """ A class for constructing functions from strings.

        Produces a function named self.name. Immediately writes
        self.documentation and closes out with self.body.
    """

    __metaclass__ = ABCMeta

    def __init__(self, name, comment_string):
        self.name = name
        self.__generated = False
        self.__source = ""          # generated source code
        self.__numbered_source = "" # numbered source code (for debugging)

        self.__arguments = []       # list of arguments
        self.__documentation = []   # list of generators
        self.__body = []            # list of generators

        # number of spaces for indentation
        self.__indent = 4*' '
        # comment characters
        self.__comment = comment_string
        super(Function, self).__init__()

    @property
    def indent(self):
        return self.__indent

    @property
    def comment(self):
        return self.__comment

    @property
    def source(self):
        if not self.__generated: self.create()
        return self.__source

    @property
    def numbered_source(self):
        if not self.__generated: self.create()
        return self.__numbered_source

    def document(self, lines):
        """ Adds a line or lines of documentation
        """
        if not self.__generated:
            self.__documentation.append("{:}{:} {:}".format(self.indent, self.comment, line).rstrip() for line in iterable_line(lines))
        else:
            raise Exception("Function document: Cannot add documentation to already generated function.")

    def add_lines(self, lines):
        """ Add a line or lines of source code
        """
        if not self.__generated:
            self.__body.append("{:}{:}".format(self.indent, line).rstrip() for line in iterable_line(lines))
        else:
            raise Exception("Function add_lines: Cannot add lines to already generated function.")

    def add_arguments(self, arguments):
        """ Add an argument or arguments to the function
        """
        if not self.__generated:
            self.__arguments.append(iterable_line(arguments))
        else:
            raise Exception("Function add_arguments: Cannot add arguments to already generated function.")

    def newline(self):
        """ Adds a new line to the source code
        """
        self.add_lines("")

    def add_comment(self, lines):
        """ Adds comments lines to the source code
        """
        self.add_lines("{:} {:}".format(self.comment, line) for line in iterable_line(lines))

    def reset(self):
        self.__generated = False

    def create(self):
        if not self.__generated:
            self.generate_source()
            self.__generated = True

    @abstractmethod
    def generate_source(self):
        pass

class PythonFunction(Function):
    def __init__(self, name, *args, **kwargs):
        super(PythonFunction, self).__init__(name, comment_string = '#', *args, **kwargs)

    def generate_source(self):
        arguments = list(chain.from_iterable(self._Function__arguments))
        documentation = list(chain.from_iterable(self._Function__documentation))
        body = list(chain.from_iterable(self._Function__body))
        prototype = ["def {:}({:}):".format(self.name, ', '.join(arguments))]
        code = prototype + documentation + body

        self._Function__source = '\n'.join(code)
        self._Function__numbered_source = '\n'.join("{:<4} {:}".format(lineno, line) for lineno, line in enumerate(code))
        
        print self._Function__source
        # now create its bytecode
        exec self._Function__source in vars()
        self.generated_func = vars()[self.name]

    def __call__(self, *args, **kwargs):
        if not self._Function__generated: self.create()
        return self.generated_func(*args, **kwargs)


class MatlabFunction(Function):
    def __init__(self, name, ret_args, *args, **kwargs):
        super(MatlabFunction, self).__init__(name, comment_string = '%', *args, **kwargs)
        self.ret_args = ret_args

    def generate_source(self):
        arguments = list(chain.from_iterable(self._Function__arguments))
        documentation = list(chain.from_iterable(self._Function__documentation))
        body = list(chain.from_iterable(self._Function__body))
        prototype = ["function [{:}] = {:}({:})".format(', '.join(self.ret_args), self.name, ', '.join(arguments))]
        code = prototype + documentation + body + ["end"]

        self._Function__source = '\n'.join(code)
        self._Function__numbered_source = '\n'.join("{:<4} {:}".format(lineno, line) for lineno, line in enumerate(code))


class CFunction(Function):
    def __init__(self, name, ret_type = "void", *args, **kwargs):
        super(CFunction, self).__init__(name, comment_string = '//', *args, **kwargs)
        self.ret_type = ret_type

    def generate_source(self):
        arguments = list(chain.from_iterable(self._Function__arguments))
        documentation = list(chain.from_iterable(self._Function__documentation))
        body = list(chain.from_iterable(self._Function__body))
        prototype = ["{:} {:}({:}) {{".format(self.ret_type, self.name, ', '.join(arguments))]
        code = prototype + documentation + body + ["}"]

        self._Function__source = '\n'.join(code)
        self._Function__numbered_source = '\n'.join("{:<4} {:}".format(lineno, line) for lineno, line in enumerate(code))



