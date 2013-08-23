from abc import ABCMeta

# create a bunch of code objects
# make sure the python, C, matlab encoder prints what you expect

class A(object): pass
class B(object): pass
class G(object): pass

class Printer(object):
    __metaclass__ = ABCMeta

    def __init__(self, print_methods):
        self.print_methods = print_methods
        super(Printer, self).__init__()

    def __call__(self, obj):
        printer = self.print_methods.get(obj.__class__, obj.__str__)
        return printer()

def python_A():
    return "print A"
def python_B():
    return "print B"

def C_A():
    return "printf('A');"
def C_B():
    return "printf('B');"

class PythonPrinter(Printer):
    def __init__(self):
        lookup = {A: python_A, B: python_B}
        super(PythonPrinter,self).__init__(lookup)

class CPrinter(Printer):
    def __init__(self):
        lookup = {A: C_A, B: C_B}
        super(CPrinter,self).__init__(lookup)


p1 = PythonPrinter()
p2 = CPrinter()

some_list = (A(), B(), A(), A(), B(), A(), G())

for e in some_list:
    print p1(e)
    print p2(e)

