from qc_parser import QCParser
from qc_rewrite import QCRewriter
from codegens import PythonCodegen, \
    MatlabCodegen, \
    C_Codegen
from helpers import profile, default_locals, convert_to_cvxopt

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

supported_languages = {
    "C": C_Codegen,
    "python": PythonCodegen,
    "matlab": MatlabCodegen
}

class QCML(object):
    """
        Must set .dims externally.
    """

    def __init__(self, debug = False):
        self.debug = debug
        self.state = ParseState.PARSE

        self.__problem_tree = None
        self.__codegen = None
        self.__dims = {}

        self.__old_mode = ""   # used to determine if codegen mode has changed

        self.problem = None
        self.prob2socp = NotImplemented
        self.socp2prob = NotImplemented

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

    @profile
    def canonicalize(self):
        if self.state > ParseState.CANONICALIZE: return
        if self.state is ParseState.CANONICALIZE:
            self.__problem_tree = QCRewriter().visit(self.__problem_tree)
            if self.debug: print self.__problem_tree
            self.state = ParseState.CODEGEN
        else:
            raise Exception("QCML canonicalize: No problem currently parsed.")

    @property
    def dims(self):
        return self.__dims

    @dims.setter
    def dims(self, dims):
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
    def codegen(self,mode="python", *args, **kwargs):
        if self.state is ParseState.COMPLETE:
            if mode != self.__old_mode: self.state = ParseState.CODEGEN
            else: return
        if self.state is ParseState.CODEGEN and self.__dims:
            def codegen_err(dims, *args, **kwargs):
                raise Exception("QCML codegen: Invalid code generator. Must be one of: ", supported_languages.keys())

            self.__codegen = supported_languages.get(mode, codegen_err)(dims = self.__dims, *args, **kwargs)
            self.__codegen.visit(self.__problem_tree)

            # save the prob2socp and socp2prob functions
            function_objs = self.__codegen.codegen()
            self.prob2socp = function_objs['prob2socp']
            self.socp2prob = function_objs['socp2prob']

            if self.debug:
                if hasattr(self.prob2socp, 'numbered_source'):
                    print self.prob2socp.numbered_source
                    print
                    print self.socp2prob.numbered_source
                    print

            self.state = ParseState.COMPLETE
            self.__old_mode = mode
        else:
            if self.state is ParseState.PARSE:
                raise Exception("QCML codegen: No problem currently parsed.")
            if self.state is ParseState.CANONICALIZE:
                raise Exception("QCML codegen: No problem currently canonicalized.")
            if not self.__dims:
                raise Exception("QCML codegen: No dimensions currently given. Please set the dimensions of the QCML object.")

    @property
    def solver(self):
        if self.state is ParseState.COMPLETE:
            try:
                import cvxopt.solvers
            except ImportError:
                raise ImportError("QCML solver: To generate a solver, requires cvxopt.")

            def f(params):
                params = convert_to_cvxopt(params)
                data = self.prob2socp(params)
                sol = cvxopt.solvers.conelp(**data)
                result = self.socp2prob(sol['x'])
                result['info'] = sol

                # set the objective value
                multiplier = self.__codegen.objective_multiplier
                offset = self.__codegen.objective_offset
                result['objval'] = multiplier * sol['primal objective'] + offset
                return result
            return f
        else:
            raise Exception("QCML solver: No python code currently generated.")

    @default_locals
    def solve(self, dims, params):
        """
            .solve(locals())
            .solve(dims,params)

            Assumes all matrices and vectors are cvxopt matrices.
        """
        if self.state is ParseState.PARSE:
            raise Exception("QCML solve: No problem currently parsed.")

        self.dims = dims
        self.canonicalize()
        self.codegen("python")

        return self.solver(params)

