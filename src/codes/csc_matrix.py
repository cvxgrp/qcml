import numpy as np
import scipy.sparse as sp

# this is for C code generation
def to_C(x):
    # if is CSCMap (or put it into the dict...)
    return "{" + ", ".join(map(str,x.mapping)) + "};"


class CSCMap(object):
    """ This object contains the parameter name (a string) and its mapping
        into the global CSC data structure.

        It supports transposes.

        Since this code is part of a "compile" phase where C code is
        generated, we choose the least efficient implementation to accomplish
        our code generation. We expect the solution to fail for larger,
        sparse matrices.

        TODO: modify to support elementwise operators, i.e., 2*spmat, etc.

        If this turns out to be the bottleneck in C code generation, we can
        always speed it up via numba and numexpr. (I think.)

        This code works since if the i,j indices are sorted by row, then the
        underlying data pattern is equivalent to the CSC format. To transpose
        the matrix, we simply convert the implicit COO format into CSR and
        read out the data; it will be properly permuted for us.
    """
    def __init__(self, param_name, ij, is_transpose = False):
        """ ij is a list of nonzero ij tuples, e.g.,
                [(0,0), (0,1), ...]
        """
        self.name = param_name
        ij.sort()   # sort the ij entries
        self.i, self.j = zip(*ij)
        self.data = np.empty(len(ij))
        self.__num_filled = 0
        self.transpose = is_transpose

    def append(self, v):
        """ This sets the *data* of the CSC matrix, so 0 <= k <= nnz(pattern),
            and is in column-major order.
        """
        self.data[self.__num_filled] = v
        self.__num_filled += 1

    @property
    def cname(self):
        return "params.%s" % self.name  # could be pointer, double, etc.

    @property
    def mapping(self):
        if self.transpose:
            # this step shuffles the data by constructing a CSR matrix
            # using the implicit COO format
            self.data = sp.csr_matrix((self.data, (self.j, self.i))).data
            self.transpose = False
        return self.data
