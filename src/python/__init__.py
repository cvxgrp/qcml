from parser import Scoop
from profiler import print_prof_data
import codegen.embed_cvxopt as g1
import codegen.embed_cvxopt_fixed_soc as g2

# to make sure atoms get loaded
import atoms

# attach the code generator to the Scoop class
from types import MethodType
Scoop.generate = MethodType(g1.generate, None, Scoop)
Scoop.generate_fixed_soc = MethodType(g2.generate, None, Scoop)

# Evaluator.add = MethodType(op_add, None, Evaluator)
# Evaluator.sub = MethodType(op_minus, None, Evaluator)
# Evaluator.mul = MethodType(op_mult, None, Evaluator)
# Evaluator.neg = MethodType(op_neg, None, Evaluator)


