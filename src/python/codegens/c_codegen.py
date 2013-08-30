"""
Code generator for ECOS C folder.

Spits out

Makefile
socp2prob.(c/h)
prob2socp.(c/h)

Links with ECOS library.
"""
from mixins.restricted_multiply import RestrictedMultiply

import qcml.expressions.expression as expression
import qcml.properties.shape as shape
from qcml.properties.curvature import isconstant
from qcml.codes.function import CFunction

class C_Codegen(RestrictedMultiply):
    """ This produces two functions and a header file.
    """
    def __init__(self, sparsity_pattern, dims = None, name = "solver"):
        super(C_Codegen, self).__init__(dims)
        self.sparsity_patterns = sparsity_pattern
        self.name = name
        
        self.__prob2socp = CFunction(self.name + "_prob_to_socp")
        self.__socp2prob = CFunction(self.name + "_socp_to_prob")

    @property
    def prob2socp(self):
        return self.__prob2socp

    @property
    def socp2prob(self):
        return self.__socp2prob
    
