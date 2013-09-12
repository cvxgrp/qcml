from qc_parser import QCParser
from qc_rewrite import QCRewriter
from codegens import PythonCodegen, \
    MatlabCodegen, \
    C_Codegen
from helpers import profile, default_locals
from errors import QC_DCPError

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

# TODO: add custom Exception classes
# TODO: add test cases for ensuring that errors are properly triggered in
# all cases

class QCML(object):
    """
        Must set .dims externally.
    """

    def __init__(self, debug = False):
        QCRewriter.varcount = 0 # reset varcount
        self.debug = debug
        self.state = ParseState.PARSE

        self.__problem_tree = None
        self.__codegen = None
        self.__dims = {}

        # keep track of the codegen language
        self.language = ""

        self.problem = None
        self.prob2socp = NotImplemented
        self.socp2prob = NotImplemented

    @profile
    def parse(self,text):
        self.__problem_tree = QCParser().parse(text)
        if self.debug: self.__problem_tree.show()

        if not self.__problem_tree.is_dcp:
            raise QC_DCPError("QCML parse: The problem is not DCP compliant.")
            # TODO: if debug, walk the tree and find the problem
            self.__problem_tree = None
        else:
            self.problem = text + "\n"
            self.state = ParseState.CANONICALIZE

    @profile
    def canonicalize(self):
        if self.state > ParseState.CANONICALIZE: return
        if self.state is ParseState.PARSE:
            raise Exception("QCML canonicalize: No problem currently parsed.")
            
        self.__problem_tree = QCRewriter().visit(self.__problem_tree)
        if self.debug: print self.__problem_tree
        self.state = ParseState.CODEGEN

    @property
    def dims(self):
        return self.__dims

    @dims.setter
    def dims(self, dims):
        if self.state is ParseState.PARSE:
            raise Exception("QCML set_dims: No problem currently parsed.")
            
        required_dims = self.__problem_tree.dimensions

        if not required_dims.issubset(set(dims.keys())):
            raise Exception("QCML set_dims: Not all required dims are supplied.")

        if any(type(dims[k]) != int for k in required_dims):
            raise Exception("QCML set_dims: Not all supplied dims are integer.")

        if self.__dims and all(dims[k] == self.__dims[k] for k in required_dims): return

        self.__dims = dims
        if self.state is ParseState.COMPLETE: self.state = ParseState.CODEGEN

    @profile
    def codegen(self, language="python", *args, **kwargs):
        if self.state is ParseState.COMPLETE:
            if language != self.language: self.state = ParseState.CODEGEN
            else: return
        if self.state is ParseState.PARSE:
            raise Exception("QCML codegen: No problem currently parsed.")
        if self.state is ParseState.CANONICALIZE:
            raise Exception("QCML codegen: No problem currently canonicalized.")
        if not self.__dims:
            raise Exception("QCML codegen: No dimensions currently given. Please set the dimensions of the QCML object.")     
        
        try:
            codegen_class = supported_languages[language]
            self.__codegen = codegen_class(dims = self.__dims, *args, **kwargs)
            self.__codegen.visit(self.__problem_tree)
        except KeyError:
            raise Exception("QCML codegen: Invalid code generator. Must be one of: ", supported_languages.keys())

        # save the prob2socp and socp2prob functions
        self.__codegen.codegen()
        self.prob2socp = self.__codegen.prob2socp
        self.socp2prob = self.__codegen.socp2prob

        if self.debug and hasattr(self.prob2socp, 'numbered_source'):
                print self.prob2socp.numbered_source
                print
                print self.socp2prob.numbered_source
                print

        self.state = ParseState.COMPLETE
        self.language = language    # set our language
            
    @property
    def solver(self):
        if self.language != "python":
            raise Exception("QCML solver: Cannot execute code generated in %s" % self.language)
        if self.state is not ParseState.COMPLETE:
            raise Exception("QCML solver: No python code currently generated.")
            
        try:
            import ecos
        except ImportError:
            raise ImportError("QCML solver: To generate a solver, requires ecos.")

        def f(params):
            data = self.prob2socp(params)
            sol = ecos.solve(**data)
            result = self.socp2prob(sol['x'])
            result['info'] = sol['info']

            # set the objective value
            multiplier = self.__codegen.objective_multiplier
            offset = self.__codegen.objective_offset
            result['objval'] = multiplier * sol['info']['pcost'] + offset
            return result
        return f            

    @default_locals
    def solve(self, params, dims = None):
        """
            .solve(locals())
            .solve(dims,params)

            Assumes all matrices and vectors are cvxopt matrices.
        """
        # TODO: what happens if we call solve after codegen(C)?
        if self.state is ParseState.PARSE:
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

