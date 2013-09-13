from . qc_parser import QCParser
from . qc_rewrite import QCRewriter
from . codegens import PythonCodegen, \
    MatlabCodegen, \
    C_Codegen
from . helpers import profile, default_locals
from . errors import QC_DCPError
from . ast.expressions import Variable

PARSE, CANONICALIZE, CODEGEN, COMPLETE = range(4)

SUPPORTED_LANGUAGES = {
    "C": C_Codegen,
    "python": PythonCodegen,
    "matlab": MatlabCodegen
}

# TODO: add custom Exception classes
# TODO: add test cases for ensuring that errors are properly triggered in
# all cases

class QCML(object):
    """ Must set .dims externally.
    """

    def __init__(self, debug = False):
        self.debug = debug
        self.state = PARSE

        self.__problem_tree = None
        self.__codegen = None
        self.__dims = {}

        # keep track of the codegen language
        self.language = ""

    @property
    def prob2socp(self):
        return self.__codegen.prob2socp

    @property
    def socp2prob(self):
        return self.__codegen.socp2prob

    @profile
    def parse(self, text):
        """ Parse state enum.

            The parser moves from the EMPTY to the PARSED to the CANONICALIZED to
            the CODEGEN state.
        """
        Variable.count = 0  # reset the variable count
        self.__problem_tree = QCParser().parse(text)
        if self.debug:
            self.__problem_tree.show()

        if not self.__problem_tree.is_dcp:
            # TODO: if debug, walk the tree and find the problem
            raise QC_DCPError("QCML parse: The problem is not DCP compliant.")
        self.state = CANONICALIZE

    @profile
    def canonicalize(self):
        if self.state > CANONICALIZE: return
        if self.state is PARSE:
            raise Exception("QCML canonicalize: No problem currently parsed.")

        self.__problem_tree.canonicalize()
        if self.debug: print self.__problem_tree
        self.state = CODEGEN

    @property
    def dims(self):
        return self.__dims

    @dims.setter
    def dims(self, dims):
        if self.state is PARSE:
            raise Exception("QCML set_dims: No problem currently parsed.")

        required_dims = self.__problem_tree.dimensions

        if not required_dims.issubset(set(dims.keys())):
            raise Exception("QCML set_dims: Not all required dims are supplied.")

        if any(type(dims[k]) != int for k in required_dims):
            raise Exception("QCML set_dims: Not all supplied dims are integer.")

        self.__dims = dims
        if self.state is COMPLETE: self.state = CODEGEN

    @profile
    def codegen(self, language="python", *args, **kwargs):
        if self.state is COMPLETE:
            if language != self.language:
                self.state = CODEGEN
            else:
                return
        if self.state is PARSE:
            raise Exception("QCML codegen: No problem currently parsed.")
        if self.state is CANONICALIZE:
            raise Exception("QCML codegen: No problem currently canonicalized.")
        if not self.__dims:
            raise Exception("QCML codegen: No dimensions currently given. Please set the dimensions of the QCML object.")

        try:
            codegen_class = SUPPORTED_LANGUAGES[language]
        except KeyError:
            raise Exception("QCML codegen: Invalid code generator. Must be one of: ", SUPPORTED_LANGUAGES.keys())
        else:
            self.__codegen = codegen_class(dims = self.__dims, *args, **kwargs)
            self.__codegen.visit(self.__problem_tree)

        # generate the prob2socp and socp2prob functions
        self.__codegen.codegen()

        if self.debug and hasattr(self.prob2socp, 'numbered_source'):
            print self.prob2socp.numbered_source
            print
            print self.socp2prob.numbered_source
            print

        self.state = COMPLETE
        self.language = language    # set our language

    @property
    def solver(self):
        if self.language != "python":
            raise Exception("QCML solver: Cannot execute code generated in %s" % self.language)
        if self.state is not COMPLETE:
            raise Exception("QCML solver: No python code currently generated.")

        try:
            import ecos
        except ImportError:
            raise ImportError("QCML solver: To generate a solver, requires ecos.")

        def solve_func(params):
            data = self.prob2socp(params)
            sol = ecos.solve(**data)
            result = self.socp2prob(sol['x'])
            result['info'] = sol['info']

            # set the objective value
            multiplier = self.__codegen.objective_multiplier
            offset = self.__codegen.objective_offset
            result['objval'] = multiplier * sol['info']['pcost'] + offset
            return result
        return solve_func

    @default_locals
    def solve(self, params, dims = None):
        """
            .solve(locals())
            .solve(dims,params)

            Assumes all matrices and vectors are cvxopt matrices.
        """
        # TODO: what happens if we call solve after codegen(C)?
        if self.state is PARSE:
            raise Exception("QCML solve: No problem currently parsed.")

        try:
            # attempt to set the dims; if this fails, usually means someone
            # set the dims externally and just wants to solve with the params
            self.dims = dims if dims else params
        except:
            raise Exception("QCML solve: Perhaps you've already canonicalized and/or generated code. Call .solver instead.")
        self.canonicalize()
        self.codegen("python")

        return self.solver(params)
