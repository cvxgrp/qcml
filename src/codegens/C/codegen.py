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
import os, shutil, site, math
from .. base_codegen import Codegen

from ... mixins import RestrictedMultiplyMixin

from ... ast.expressions import expression
from ... properties import shape
from ... properties.curvature import isconstant

from ... codes import OnesCoeff, ConstantCoeff
from ... codes.function import CFunction
from ... codes.encoders import toC


def write_template(template_file, new_file, code):
    with open(template_file, 'r') as template, open(new_file, 'w') as output:
        for line in template:
            output.write(line % (code))

def shape_to_c_type(x):
    if shape.isscalar(x): return "double"
    if shape.isvector(x): return "double *"
    if shape.ismatrix(x): return "qc_matrix *"
    raise Exception("Unknown shape...")

class C_Codegen(RestrictedMultiplyMixin, Codegen):
    """ This produces two functions and a header file.
    """
    def __init__(self, sparsity_pattern = None, name = "problem", *args, **kwargs):
        super(C_Codegen, self).__init__(*args, **kwargs)
        # TODO: allow optimizations with given sparsity pattern
        self.sparsity_patterns = sparsity_pattern
        self.name = name

        # functions we are going to generate
        self._code = {}
        self._code['prob2socp'] = CFunction("qc_%s2socp" % self.name,
            arguments = ["const %s_params * params" % self.name, 
                         "const %s_dims * dims" % self.name],
            ret_type="qc_socp *")
        self._code['socp2prob'] = CFunction("qc_socp2%s" % self.name,
            arguments = ["double * x", "%s_vars * vars" % self.name,
                         "const %s_dims * dims" % self.name])
        self._codekeyorder = ['prob2socp', 'socp2prob']

        # get the paths to the template files
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

        # parameters and variables in the optimization problem
        self.params = ""
        self.abstract_dims = ""
        self.variables = ""
        self.indent = self.prob2socp.indent   # set our indent spacing

        # keep track of the total nonzeros in each matrix
        self.nnz = {'G': [], 'A': []}

        # keep track of the problem dimension
        self.size_lookup = {'m': 0, 'n': 0, 'p': 0}

    @property
    def prob2socp(self): return self.code['prob2socp']

    @property
    def socp2prob(self): return self.code['socp2prob']

    def codegen(self):
        super(C_Codegen, self).codegen()

        # create the dictionary for the generated code
        if not os.path.exists(self.new_dir):
            os.makedirs(self.new_dir)

        # populate the dict needed for codegen
        codegen_dict = {
            'name': self.name,
            'NAME': self.name.upper(),
            'params': self.params,
            'dims': self.abstract_dims,
            'variables': self.variables,
            'prob2socp': self.prob2socp.source,
            'socp2prob': self.socp2prob.source,
            'prob2socp_prototype': self.prob2socp.prototype,
            'socp2prob_prototype': self.socp2prob.prototype
        }

        # copy over the static utility files
        shutil.copy("%s/qcml_utils.c" % self.data_dir, self.new_dir)
        shutil.copy("%s/qcml_utils.h" % self.data_dir, self.new_dir)

        # write out the files
        write_template(self.makefile_template, self.makefile, codegen_dict)
        write_template(self.header_file_template, self.header_file, codegen_dict)
        write_template(self.source_file_template, self.source_file, codegen_dict)

    # generator to get cone sizes
    def c_dimensions(self):
        self.size_lookup['m'] = self.num_conic + self.num_lps
        self.size_lookup['n'] = self.num_vars
        self.size_lookup['p'] = self.num_lineqs
        yield "data->p = %s;" % self.num_lineqs
        yield "data->m = %s;" % (self.num_conic + self.num_lps)
        yield "data->n = %s;" % self.num_vars

    # generator to get cone dimensions
    def c_cone_sizes(self):
        num_cone, cone_size = zip(*self.cone_list)

        yield "data->l = %s;" % self.num_lps
        yield "data->nsoc = %s;" % sum(num_cone)
        if num_cone == 0:
            yield "data->q = NULL;"
        else:
            yield "data->q = malloc(data->nsoc * sizeof(long));"
            yield "if(!data->q) return qc_socp_free(data);"
            yield ""
            yield "/* initialize the cone */"
            yield "q_ptr = data->q;"
            for num, sz in self.cone_list:
                if num == 1: yield "*q_ptr++ = %s;" % sz
                else: yield "for(i = 0; i < %s; ++i) *q_ptr++ = %s;" % (num, sz)

    # function to get parameters
    def c_params(self):
        return ["%s%s %s;" % (self.indent, shape_to_c_type(v),k) for (k,v) in self.program.parameters.iteritems()]

    # function to get abstract dims
    def c_dims(self):
        return ["%sint %s;" % (self.indent, k) for k in self.program.abstract_dims]

    # function to get variables
    def c_variables(self):
        return ["%s%s %s;" % (self.indent, shape_to_c_type(v),k) for (k,v) in self.program.variables.iteritems()]

    # generator to allocate socp data structures
    def c_allocate_socp(self):
        yield "qc_socp * data = calloc(1, sizeof(qc_socp));"
        yield "if (!data) return qc_socp_free(data);"

    # generator to allocate vectors
    def c_allocate_vector(self, vector, size):
        if self.size_lookup[size] == 0:
            yield "data->%s = NULL;" % vector
        else:
            yield "data->%s = calloc(data->%s, sizeof(double));" % (vector, size)
            yield "if (!data->%s) return qc_socp_free(data);" % (vector)

    def c_allocate_matrix(self, matrix):
        const = sum(int(x) for x in self.nnz[matrix] if x.isdigit())
        size = ' + '.join(x for x in self.nnz[matrix] if not x.isdigit())
        if const > 0: size = "%s + %d" % (size, const)

        if const > 0 or size:
            yield "long nnz%s = %s;" % (matrix, size)
            yield "data->%(matrix)sx = malloc(nnz%(matrix)s * sizeof(double));" % {'matrix': matrix}
            yield "data->%(matrix)sp = malloc(nnz%(matrix)s * sizeof(long));" % {'matrix': matrix}
            yield "data->%(matrix)si = malloc(nnz%(matrix)s * sizeof(long));" % {'matrix': matrix}
            yield "if ((!data->%(matrix)sx) || (!data->%(matrix)sp) || (!data->%(matrix)si)) return qc_socp_free(data);" % {'matrix': matrix}
        else:
            yield "long nnz%s = 0;" % (matrix)
            yield "data->%sx = NULL;" % (matrix)
            yield "data->%sp = NULL;" % (matrix)
            yield "data->%si = NULL;" % (matrix)
        yield "%(matrix)s_data_ptr = data->%(matrix)sx;" % {'matrix': matrix}
        yield "%(matrix)s_row_ptr = data->%(matrix)si;" % {'matrix': matrix}
        yield "%(matrix)s_col_ptr = data->%(matrix)sp;" % {'matrix': matrix}

    def c_setup_qc_matrix(self, matrix):
        if self.nnz[matrix]:
            yield "qc_matrix *%s_coo = malloc(sizeof(qc_matrix));" % matrix
            yield "if (!%s_coo) return qc_socp_free(data);" % matrix
            yield "%s_coo->m = data->m; %s_coo->n = data->n; %s_coo->nnz = nnz%s;" % (matrix, matrix, matrix, matrix)
            yield "%s_coo->i = data->%si;" % (matrix, matrix)
            yield "%s_coo->j = data->%sp;" % (matrix, matrix)
            yield "%s_coo->v = data->%sx;" % (matrix, matrix)

    def c_compress(self, matrix):
        if self.nnz[matrix]:
            yield "qc_matrix *%s_csc = qc_compress(%s_coo);" % (matrix, matrix)
            yield "if (!%s_csc) return qc_socp_free(data);" % matrix
            yield ""
            yield "/* free the old memory */"
            yield "qc_spfree(%s_coo);" % matrix
            yield ""
            yield "/* reassign into data, pointer now owned by data */"
            yield "data->%si = %s_csc->i;" % (matrix, matrix)
            yield "data->%sp = %s_csc->j;" % (matrix, matrix)
            yield "data->%sx = %s_csc->v;" % (matrix, matrix)

    def c_recover(self):
        for k,v in self.program.variables.iteritems():
            if shape.isscalar(v):
                yield "vars->%s = *(x + %s);" % (k, self.varstart[k])
            else:
                yield "vars->%s = x + %s;  /* length %s */" % (k, self.varstart[k], self.varlength[k])


    def functions_setup(self):
        # add some documentation
        self.prob2socp.document("maps 'params' into the C socp data type")
        self.prob2socp.document("'params' ought to contain:")
        self.prob2socp.document(self.printshapes(self.program))
        self.prob2socp.newline()

        self.params = '\n'.join(self.c_params())
        self.abstract_dims = '\n'.join(self.c_dims())
        self.variables = '\n'.join(self.c_variables())

        self.prob2socp.add_comment("local variables")
        self.prob2socp.add_lines("long i;  /* loop index */")
        self.prob2socp.add_lines("long *q_ptr;")
        self.prob2socp.add_lines("long *A_row_ptr, *A_col_ptr;")
        self.prob2socp.add_lines("long *G_row_ptr, *G_col_ptr;")
        self.prob2socp.add_lines("double *A_data_ptr, *G_data_ptr;")
        self.prob2socp.newline()
        self.prob2socp.add_comment("allocate socp data structure")
        self.prob2socp.add_lines(self.c_allocate_socp())
        self.prob2socp.newline()

        # set up the data structures
        self.prob2socp.add_comment("allocate problem dimensions")
        self.prob2socp.add_lines(self.c_dimensions())
        self.prob2socp.newline()

        self.prob2socp.add_comment("allocate c vector")
        self.prob2socp.add_lines(self.c_allocate_vector("c", "n"))
        self.prob2socp.newline()

        self.prob2socp.add_comment("allocate h vector")
        self.prob2socp.add_lines(self.c_allocate_vector("h", "m"))
        self.prob2socp.newline()

        self.prob2socp.add_comment("allocate b vector")
        self.prob2socp.add_lines(self.c_allocate_vector("b", "p"))
        self.prob2socp.newline()

        self.prob2socp.add_comment("allocate G matrix")
        self.prob2socp.add_lines(self.c_allocate_matrix("G"))
        self.prob2socp.newline()

        self.prob2socp.add_comment("allocate A matrix")
        self.prob2socp.add_lines(self.c_allocate_matrix("A"))
        self.prob2socp.newline()

        self.prob2socp.add_comment("allocate the cone sizes")
        self.prob2socp.add_lines(self.c_cone_sizes())

    def functions_return(self):
        #self.prob2socp.add_lines("""for(i=0; i< 16; ++i) printf("%f ", data->Gx[i]);""")
        self.prob2socp.add_comment("convert G and A ptrs into a qc_matrix")
        # creates an object named "G_coo"
        self.prob2socp.add_lines(self.c_setup_qc_matrix("G"))
        # creates an object named "A_coo"
        self.prob2socp.add_lines(self.c_setup_qc_matrix("A"))
        self.prob2socp.newline()
        self.prob2socp.add_comment("convert the matrices to column compressed form")
        self.prob2socp.add_lines(self.c_compress("G"))
        self.prob2socp.add_lines(self.c_compress("A"))
        self.prob2socp.add_lines("return data;")

        self.socp2prob.document("recovers the problem variables from the solver variable 'x'")
        self.socp2prob.document("assumes the variables struct is externally allocated")
        self.socp2prob.document("the user must keep track of the variable length;")
        # recover the old variables
        self.socp2prob.add_lines(self.c_recover())

    def stuff_c(self, start, end, expr):
        # TODO: i shouldn't have to check here....
        if expr.isscalar or isinstance(expr, OnesCoeff): tag = ";"
        else: tag = "[i];"
        yield "for(i = 0; i < %s; ++i) data->c[i + %s] = %s%s" % (end-start, start, toC(expr), tag)

    def stuff_b(self, start, end, expr):
        # TODO: i shouldn't have to check here....
        if expr.isscalar or isinstance(expr, OnesCoeff): tag = ";"
        else: tag = "[i];"
        yield "for(i = 0; i < %s; ++i) data->b[i + %s] = %s%s" % (end-start, start, toC(expr), tag)

    def stuff_h(self, start, end, expr, stride = None):
        if expr.isscalar: tag = ";"
        else: tag = "[i];"
        if stride is not None and stride != 1:
            numel = math.ceil( float(end - start) / stride )
            yield "for(i = 0; i < %d; ++i) data->h[%s * i + %s] = %s%s" % (numel, stride, start, toC(expr), tag)
        else:
            yield "for(i = 0; i < %s; ++i) data->h[i + %s] = %s%s" % (end-start, start, toC(expr), tag)


    def stuff_matrix(self, matrix, row_start, row_end, col_start, col_end, expr, row_stride):
        yield toC(expr.I(row_start, row_stride)) % ({'ptr': "%s_row_ptr" % matrix})
        yield toC(expr.J(col_start)) % ({'ptr': "%s_col_ptr" % matrix})
        yield toC(expr.V()) % ({'ptr': "%s_data_ptr" % matrix})

    def stuff_G(self, row_start, row_end, col_start, col_end, expr, row_stride = 1):
        # execute this code first
        self.nnz['G'].append(toC(expr.nnz()))
        n = (row_end - row_start)/row_stride
        if n > 1 and expr.isscalar: expr = OnesCoeff(n,ConstantCoeff(1))*expr

        # but then return this generator
        return self.stuff_matrix("G", row_start, row_end, col_start, col_end, expr, row_stride)

    def stuff_A(self, row_start, row_end, col_start, col_end, expr, row_stride = 1):
        # execute this code first
        self.nnz['A'].append(toC(expr.nnz()))
        n = (row_end - row_start)/row_stride
        if n > 1 and expr.isscalar: expr = OnesCoeff(n,ConstantCoeff(1))*expr

        # but then return this generator
        return self.stuff_matrix("A", row_start, row_end, col_start, col_end, expr, row_stride)

    def abstractdim_rewriter(self, ad):
        return "dims->%s" % ad
