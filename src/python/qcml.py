from qc_parser import QCParser
from qc_rewrite import QCRewriter
from qc_codegen import CVXCodegen, CVXOPTCodegen, ECOSCodegen, MatlabCodegen

class QCML(object):
    codegen_objects = {
        "cvx": CVXCodegen,
        "cvxopt": CVXOPTCodegen, 
        "ecos": ECOSCodegen,
        "matlab": MatlabCodegen
    }
    
    python_solvers = set(["cvxopt", "ecos"])
    
    def __init__(self, debug = False):
        self.debug = debug
        self.rewritten = False
        
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
    
    def parse(self,text):
        self.__problem_tree = self.__parser.parse(text)
        if self.debug:
            self.__problem_tree.show()
            
        self.problem = text + "\n"
        self.rewritten = False
    
    def append(self, text):
        if self.problem is not None:
            self.problem += text
            self.__problem_tree = self.__parser.parse(self.problem)
        else:
            print "QCML append: No problem currently parsed."
        
    def rewrite(self):
        if self.rewritten:
            # already rewritten
            return
        # almost idempotent, except that norm(x) <= t gets rewritten
        if self.__problem_tree is not None:
            self.__problem_tree = self.__rewriter.visit(self.__problem_tree)
            if self.debug:
                print self.__problem_tree
            self.rewritten = True
        else:
            print "QCML rewrite: No problem currently parsed."
    
    def codegen(self,mode="cvx", **kwargs):
        def codegen_err():
            print "QCML codegen: Invalid code generator. Must be one of: ", codegen_objects.keys()
        
        if self.__problem_tree is not None:
            if self.rewritten:
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
        self.rewrite()
        self.codegen(mode)
        
        return self.solver
        
    def solve(self, text, mode="cvxopt", **kwargs):
        if mode in self.python_solvers:
            self.create_solver(text, mode)
            return self.solver(**kwargs)
        else:
            print "QCML solve: Not a python solver."
    
        