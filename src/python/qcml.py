from qc_parser import QCParser
from qc_rewrite import QCRewriter
from qc_codegen import CVXCodegen, CVXOPTCodegen, ECOSCodegen, MatlabCodegen, PDOSCodegen

class QCML(object):
    codegen_objects = {
        "cvx": CVXCodegen,
        "cvxopt": CVXOPTCodegen,
        "ecos": ECOSCodegen,
        "matlab": MatlabCodegen,
        "pdos": PDOSCodegen
    }

    python_solvers = set(["cvxopt", "ecos", "pdos"])

    def __init__(self, debug = False):
        self.debug = debug
        self.canonical = False

        self.__parser = QCParser()
        self.__rewriter = QCRewriter()

        self.__problem_tree = None
        self.__codegen = None

        self.problem = None
        self.solver = None

    def prettyprint(self,lineno=False):
        if self.__codegen is not None:
            self.__codegen.prettyprint(lineno)
        else:
            print "QCML prettyprint: No code generated yet."

    def print_canon(self):
        if self.canonical:
            print self.__problem_tree
        else:
            self.canonicalize()
            print self.__problem_tree

    def parse(self,text):
        self.__problem_tree = self.__parser.parse(text)
        if self.debug:
            self.__problem_tree.show()

        if not self.__problem_tree.is_dcp:
            print "QCML parse: The problem is not DCP compliant."
            # TODO: if debug, walk the tree and find the problem
            self.__problem_tree = None
        else:
            self.problem = text + "\n"
            self.canonical = False

    def append(self, text):
        if self.problem is not None:
            self.problem += text
            self.__problem_tree = self.__parser.parse(self.problem)
        else:
            print "QCML append: No problem currently parsed."

    def canonicalize(self):
        if self.canonical:
            # already canonicalized
            return
        if self.__problem_tree is not None:
            self.__problem_tree = self.__rewriter.visit(self.__problem_tree)
            if self.debug:
                print self.__problem_tree
            self.canonical = True
        else:
            print "QCML rewrite: No problem currently parsed."

    def codegen(self,mode="cvx", **kwargs):
        def codegen_err():
            print "QCML codegen: Invalid code generator. Must be one of: ", codegen_objects.keys()

        if self.__problem_tree is not None:
            if self.canonical:
                self.__codegen = self.codegen_objects.get(mode, codegen_err)(**kwargs)
                self.__codegen.visit(self.__problem_tree)

                if self.debug:
                    self.__codegen.prettyprint(True)

                if mode in self.python_solvers:
                    self.solver = self.__codegen.codegen()
                else:
                    self.solver = None
            else:
                print "QCML codegen: No problem currently rewritten."
        else:
            print "QCML codegen: No problem currently parsed."

    def create_solver(self,text,mode="cvxopt"):
        self.parse(text)
        self.canonicalize()
        self.codegen(mode)

        return self.solver

    def solve(self, text, mode="cvxopt", **kwargs):
        if mode in self.python_solvers:
            self.create_solver(text, mode)
            return self.solver(**kwargs)
        else:
            print "QCML solve: Not a python solver."
