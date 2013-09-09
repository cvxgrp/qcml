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

    def __init__(self, name, prototype, comment_string):
        self.name = name
        self.__generated = False
        self.__source = ""          # generated source code

        self.prototype = prototype  # function prototype
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
        source = iter(self.source.splitlines())
        return '\n'.join("{:<4} {:}".format(lineno+1, line) for lineno, line in enumerate(source))

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
            documentation = list(chain.from_iterable(self.__documentation))
            body = list(chain.from_iterable(self.__body))
            self.__source = self._generate_source(documentation, body)
            self.__generated = True

    @abstractmethod
    def _generate_source(self, documentation, body):
        pass
    
class PythonFunction(Function):
    def __init__(self, name, arguments = []):
        prototype = "def {:}({:}):".format(name, ', '.join(arguments))
        super(PythonFunction, self).__init__(name, prototype, comment_string = '#')
    
    def _generate_source(self, documentation, body):
        # attach the body; if no body, simply create an empty function
        code = [self.prototype] + documentation + (body if body else ["%spass" % (self.indent)])
        code_str = '\n'.join(code)
        
        # now create its bytecode
        exec code_str in vars()
        self.generated_func = vars()[self.name]
        
        return code_str

    def __call__(self, *args, **kwargs):
        if not self._Function__generated: self.create()
        return self.generated_func(*args, **kwargs)


class MatlabFunction(Function):
    def __init__(self, name, arguments =[], ret_args = []):
        prototype = "function [{:}] = {:}({:})".format(', '.join(ret_args), name, ', '.join(arguments))
        super(MatlabFunction, self).__init__(name, prototype, comment_string = '%')

    def _generate_source(self, documentation, body):
        code = [self.prototype] + documentation + body + ["end"]
        return '\n'.join(code)


class CFunction(Function):
    def __init__(self, name, arguments = [], ret_type = "void", *args, **kwargs):
        prototype = "{:} {:}({:})".format(ret_type, name, ', '.join(arguments))
        super(CFunction, self).__init__(name, prototype, comment_string = '//')

    def _generate_source(self, documentation, body):
        code = [self.prototype, "{"] + documentation + body + ["}"]
        return '\n'.join(code)
        