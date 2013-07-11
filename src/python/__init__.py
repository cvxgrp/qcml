# from parser import Scoop
# from profiler import print_prof_data
# import codegen.embed_cvxopt as g1
# import codegen.embed_cvxopt_fixed_soc as g2
# import codegen.embed_matrix as g3
# import codegen.embed_fixed_matrix as g4
# import codegen.embed_pdos as g5
# import codegen.generate_scala as g6
# import codegen.embed_ecos as g7

# to make sure atoms get loaded
# import atoms

# attach the code generator to the Scoop class
# from types import MethodType
# Scoop.generate = MethodType(g1.generate, None, Scoop)
# Scoop.generate_fixed_soc = MethodType(g2.generate, None, Scoop)
# Scoop.generate_matrix = MethodType(g3.generate, None, Scoop)
# Scoop.generate_fixed_matrix = MethodType(g4.generate, None, Scoop)
# Scoop.generate_pdos = MethodType(g5.generate, None, Scoop)
# Scoop.generate_delite = MethodType(g6.generate, None, Scoop)
# Scoop.generate_ecos = MethodType(g7.generate, None, Scoop)

# Evaluator.add = MethodType(op_add, None, Evaluator)
# Evaluator.sub = MethodType(op_minus, None, Evaluator)
# Evaluator.mul = MethodType(op_mult, None, Evaluator)
# Evaluator.neg = MethodType(op_neg, None, Evaluator)


from qcml import QCML
from qc_parser import QCError

from qc_rewrite import QCRewriter
import qc_atoms
atoms = qc_atoms.atoms