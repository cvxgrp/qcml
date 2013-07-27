from qc_parser import QCParser
from qc_rewrite import QCRewriter
from qc_codegen import CVXCodegen, CVXOPTCodegen, ECOSCodegen, MatlabCodegen, PDOSCodegen

import cvxopt
import time # for benchmarking

def _convert_to_cvxopt_matrices(variables):
    try:
        import numpy as np
        import cvxopt

        cvxopt_params = {k:cvxopt.matrix(v) for k,v in variables.iteritems() if isinstance(v, np.ndarray)}
        variables.update(cvxopt_params)
    except ImportError:
        pass
    return variables

def profile(f):
    def wrap(*args, **kwargs):
        start = time.clock()
        result = f(*args, **kwargs)
        elapsed = time.clock() - start
        print f.__name__, "took", elapsed, "secs"
        return result
    return wrap

def default_locals(f):
    def wrap(self, *args, **kwargs):
        if args or kwargs:
            result = f(self, *args, **kwargs)
        else:
            # get the local calling frame
            # http://stackoverflow.com/questions/6618795/get-locals-from-calling-namespace-in-python
            import inspect
            frame = inspect.currentframe()
            try:
                variables = frame.f_back.f_locals
            finally:
                del frame

            # cast to cvxopt matrices if needed
            # if there are numpy matrices, promote them to cvxopt matrices
            variables = _convert_to_cvxopt_matrices(variables)

            result = f(self, variables, variables)
        return result
    return wrap

class ParseState(object):
    """ Parse state enum.

        The parser moves from the EMPTY to the PARSED to the CANONICALIZED to
        the CODEGEN state.
    """
    __init__ = NotImplemented

    PARSE = 0
    CANONICALIZE = 1
    CODEGEN = 2
    COMPLETE = 3

class QCML(object):
    """
        Must set .dims externally.
    """
    codegen_objects = {
        "cvx": CVXCodegen,
        "cvxopt": CVXOPTCodegen,
        "ecos": ECOSCodegen,
        "matlab": MatlabCodegen,
        "pdos": PDOSCodegen
    }

    def __init__(self, debug = False):
        self.debug = debug
        self.state = ParseState.PARSE

        self.__problem_tree = None
        self.__codegen = None
        self.__dims = {}

        self.__old_mode = ""   # used to determine if codegen mode has changed

        self.problem = None
        self.solver = None      # TODO: consider making "private"

    def prettyprint(self,lineno=False):
        if self.state is ParseState.COMPLETE:
            self.__codegen.prettyprint(lineno)
        else:
            raise Exception("QCML prettyprint: No code generated yet.")

    @profile
    def parse(self,text):
        self.__problem_tree = QCParser().parse(text)
        if self.debug: self.__problem_tree.show()

        if not self.__problem_tree.is_dcp:
            raise Exception("QCML parse: The problem is not DCP compliant.")
            # TODO: if debug, walk the tree and find the problem
            self.__problem_tree = None
        else:
            self.problem = text + "\n"
            self.state = ParseState.CANONICALIZE

    # def append(self, text):
    #     if self.problem is not None:
    #         self.problem += text
    #         self.__problem_tree = self.__parser.parse(self.problem)
    #     else:
    #         print "QCML append: No problem currently parsed."

    @profile
    def canonicalize(self):
        if self.state > ParseState.CANONICALIZE: return
        if self.state is ParseState.CANONICALIZE:
            self.__problem_tree = QCRewriter().visit(self.__problem_tree)
            if self.debug: print self.__problem_tree
            self.state = ParseState.CODEGEN
        else:
            raise Exception("QCML canonicalize: No problem currently parsed.")


    def set_dims(self, dims):
        if self.state > ParseState.PARSE:
            required_dims = self.__problem_tree.dimensions

            if not required_dims.issubset(set(dims.keys())):
                raise Exception("QCML set_dims: Not all required dims are supplied.")

            if any(type(dims[k]) != int for k in required_dims):
                raise Exception("QCML set_dims: Not all supplied dims are integer.")

            if self.__dims and all(dims[k] == self.__dims[k] for k in required_dims): return

            self.__dims = dims
            if self.state is ParseState.COMPLETE: self.state = ParseState.CODEGEN
        else:
            raise Exception("QCML set_dims: No problem currently parsed.")

    @profile
    def codegen(self,mode="cvx", **kwargs):
        if self.state is ParseState.COMPLETE:
            if mode != self.__old_mode: self.state = ParseState.CODEGEN
            else: return
        if self.state is ParseState.CODEGEN and self.__dims:
            def codegen_err(dims, **kwargs):
                raise Exception("QCML codegen: Invalid code generator. Must be one of: ", self.codegen_objects.keys())

            self.__codegen = self.codegen_objects.get(mode, codegen_err)(self.__dims, **kwargs)
            self.__codegen.visit(self.__problem_tree)

            if self.debug:
                self.__codegen.prettyprint(True)

            self.solver = self.__codegen.codegen()
            self.state = ParseState.COMPLETE
            self.__old_mode = mode
        else:
            if self.state is ParseState.PARSE:
                raise Exception("QCML codegen: No problem currently parsed.")
            if self.state is ParseState.CANONICALIZE:
                raise Exception("QCML codegen: No problem currently canonicalized.")
            if not self.__dims:
                raise Exception("QCML codegen: No dimensions currently given. Please call set_dims(...).")

    @default_locals
    def solve(self, dims, params):
        """
            .solve(locals())
            .solve(dims,params)

            Assumes all matrices and vectors are cvxopt matrices.
        """

        if self.state is ParseState.PARSE:
            raise Exception("QCML solve: No problem currently parsed.")

        self.set_dims(dims)
        self.canonicalize()
        self.codegen("cvxopt")

        return self.solver(params)

    def prob2socp(self, dims, params=None):
        # spits out the socp data matrices
        pass

