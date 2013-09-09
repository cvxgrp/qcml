"""
Code generator for ECOS C folder.

Spits out

Makefile
socp2prob.(c/h)
prob2socp.(c/h)

Will copy files to a folder with name "name". Produce a Makefile that compiles
the object files. Only really need one copy of qcml_utils across all generated
code....

Need to compile qcml_utils.c to a qcml_utils.o, but only need to link it into
the matrix stuffing object.

Links with ECOS library.
"""
import os, shutil, site
from .. mixins.restricted_multiply import RestrictedMultiply

import qcml.expressions.expression as expression
import qcml.properties.shape as shape
from qcml.properties.curvature import isconstant
from qcml.codes.function import CFunction

from qcml.codes.coefficients import OnesCoeff, ConstantCoeff
import qcml.codes.encoders as encoder


def write_template(template_file, new_file, code):
    with open(template_file, 'r') as template, open(new_file, 'w') as output:
        for line in template:
            output.write(line % (code))


class C_Codegen(RestrictedMultiply):
    """ This produces two functions and a header file.
    """
    def __init__(self, sparsity_pattern = None, dims = None, name = "problem"):
        super(C_Codegen, self).__init__(dims)
        # TODO: allow optimizations with given sparsity pattern
        self.sparsity_patterns = sparsity_pattern   
        self.name = name

        self.__prob2socp = CFunction("qc_%s2socp" % self.name, 
            arguments = ["const %s_params * prob" % self.name], 
            ret_type="qc_socp *")
        self.__socp2prob = CFunction("qc_socp2%s" % self.name, 
            arguments = ["const double * x"], 
            ret_type="%s_vars *" % self.name)
        
        template_path = os.path.dirname(__file__)
        path = os.getcwd()
        
        self.makefile_template = "%(template_path)s/Makefile_template" % vars()
        self.source_file_template = "%(template_path)s/stuff_template.c" % vars()
        self.header_file_template = "%(template_path)s/stuff_template.h" % vars()
        
        new_dir = "%(path)s/%(name)s" % vars()
        self.new_dir = new_dir                  # new directory to create
        self.data_dir = template_path           # path to data files
        self.makefile = "%(new_dir)s/Makefile" % vars()
        self.source_file = "%(new_dir)s/%(name)s.c" % vars()
        self.header_file = "%(new_dir)s/%(name)s.h" % vars()


    @property
    def prob2socp(self): return self.__prob2socp

    @property
    def socp2prob(self): return self.__socp2prob
    
    def codegen(self):
        super(C_Codegen, self).codegen()
        
        # create the dictionary for the generated code
        if not os.path.exists(self.new_dir):
            os.makedirs(self.new_dir)
            
        # populate the dict needed for codegen
        codegen_dict = {
            'name': self.name,
            'params': '  double *A;\n  double c;',
            'variables': '  double *x;\n  double y;',
            'prob2socp': self.__prob2socp.source,
            'socp2prob': self.__socp2prob.source,
            'prob2socp_prototype': self.__prob2socp.prototype,
            'socp2prob_prototype': self.__socp2prob.prototype
        }
        
        # copy over the static utility files
        shutil.copy("%s/qcml_utils.c" % self.data_dir, self.new_dir)
        shutil.copy("%s/qcml_utils.h" % self.data_dir, self.new_dir)

        # write out the files
        write_template(self.makefile_template, self.makefile, codegen_dict)
        write_template(self.header_file_template, self.header_file, codegen_dict)
        write_template(self.source_file_template, self.source_file, codegen_dict)
        
    # function to get cone sizes
    def python_cone_sizes(self):
        yield "p, m, n = %d, %d, %d" % (self.num_lineqs, self.num_conic + self.num_lps, self.num_vars)

    # function to get cone dimensions
    def python_dimensions(self):
        def cone_tuple_to_str(x):
            num, sz = x
            if num == 1: return "[%s]" % sz
            else: return "%s*[%s]" % (num, sz)
        cone_list_str = '[]'
        if self.cone_list:
            cone_list_str = map(cone_tuple_to_str, self.cone_list)
            cone_list_str = '+'.join(cone_list_str)

        yield "dims = {'l': %d, 'q': %s, 's': []}" % (self.num_lps, cone_list_str)

    def functions_setup(self, program_node):
        # add some documentation
        self.prob2socp.document("maps 'params' into a dictionary of SOCP matrices")
        self.prob2socp.document("'params' ought to contain:")
        shapes = ("  '%s' has shape %s" % (v, v.shape.eval(self.dims)) for v in program_node.parameters.values())
        self.prob2socp.document(shapes)

        # now import cvxopt and itertools
        self.prob2socp.add_lines("import numpy as np")
        self.prob2socp.add_lines("import scipy.sparse as sp")
        self.prob2socp.add_lines("import itertools")
        self.prob2socp.newline()
        # self.prob2socp.add_comment("convert possible numpy parameters to cvxopt matrices")
        # self.prob2socp.add_lines("from qcml.helpers import convert_to_cvxopt")
        # self.prob2socp.add_lines("params = convert_to_cvxopt(params)")
        # self.prob2socp.newline()

        # set up the data structures
        self.prob2socp.add_lines(self.python_cone_sizes())
        self.prob2socp.add_lines("c = np.zeros((n,))")
        self.prob2socp.add_lines("h = np.zeros((m,))")
        self.prob2socp.add_lines("b = np.zeros((p,))")
        self.prob2socp.add_lines("Gi, Gj, Gv = [], [], []")
        self.prob2socp.add_lines("Ai, Aj, Av = [], [], []")
        self.prob2socp.add_lines(self.python_dimensions())

    def functions_return(self, program_node):
        # TODO: what to do when m, n, or p is 0?
        # it "just worked" with CVXOPT, but not with scipy/numpy anymore...
        self.prob2socp.add_comment("construct index and value lists for G and A")
        self.prob2socp.add_lines("Gi = np.fromiter(itertools.chain.from_iterable(Gi), dtype=np.int)")
        self.prob2socp.add_lines("Gj = np.fromiter(itertools.chain.from_iterable(Gj), dtype=np.int)")
        self.prob2socp.add_lines("Gv = np.fromiter(itertools.chain.from_iterable(Gv), dtype=np.double)")
        self.prob2socp.add_lines("Ai = np.fromiter(itertools.chain.from_iterable(Ai), dtype=np.int)")
        self.prob2socp.add_lines("Aj = np.fromiter(itertools.chain.from_iterable(Aj), dtype=np.int)")
        self.prob2socp.add_lines("Av = np.fromiter(itertools.chain.from_iterable(Av), dtype=np.double)")
        self.prob2socp.add_lines("if m > 0: G = sp.csc_matrix((Gv, np.vstack((Gi, Gj))), (m,n))")
        self.prob2socp.add_lines("else: G, h = None, None")
        self.prob2socp.add_lines("if p > 0: A = sp.csc_matrix((Av, np.vstack((Ai, Aj))), (p,n))")
        self.prob2socp.add_lines("else: A, b = None, None")
        self.prob2socp.add_lines("return {'c': c, 'G': G, 'h': h, 'A': A, 'b': b, 'dims': dims}")

        self.socp2prob.document("recovers the problem variables from the solver variable 'x'")
        # recover the old variables
        recover = (
            "'%s' : x[%s:%s]" % (k, self.varstart[k], self.varstart[k]+self.varlength[k])
                for k in program_node.variables.keys()
        )
        self.socp2prob.add_lines("return {%s}" % ', '.join(recover))

    def stuff_c(self, start, end, expr):
        yield "c[%s:%s] = %s" % (start, end, encoder.toPython(expr))

    def stuff_b(self, start, end, expr):
        yield "b[%s:%s] = %s" % (start, end, encoder.toPython(expr))

    def stuff_h(self, start, end, expr, stride = None):
        if stride is not None:
            yield "h[%s:%s:%s] = %s" % (start, end, stride, encoder.toPython(expr))
        else:
            yield "h[%s:%s] = %s" % (start, end, encoder.toPython(expr))

    def stuff_G(self, row_start, row_end, col_start, col_end, expr, row_stride = 1):
        n = (row_end - row_start)/row_stride
        if n > 1 and expr.isscalar:
            expr = OnesCoeff(n,ConstantCoeff(1))*expr
        to_sparse = expr.to_sparse()
        if to_sparse: yield encoder.toPython(to_sparse)
        yield "Gi.append(%s)" % encoder.toPython(expr.I(row_start, row_stride))
        yield "Gj.append(%s)" % encoder.toPython(expr.J(col_start))
        yield "Gv.append(%s)" % encoder.toPython(expr.V())

    def stuff_A(self, row_start, row_end, col_start, col_end, expr, row_stride = 1):
        n = (row_end - row_start)/row_stride
        if n > 1 and expr.isscalar:
            expr = OnesCoeff(n,ConstantCoeff(1))*expr
        to_sparse = expr.to_sparse()
        if to_sparse: yield encoder.toPython(to_sparse)
        yield "Ai.append(%s)" % encoder.toPython(expr.I(row_start, row_stride))
        yield "Aj.append(%s)" % encoder.toPython(expr.J(col_start))
        yield "Av.append(%s)" % encoder.toPython(expr.V())

    
    
