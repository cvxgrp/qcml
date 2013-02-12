from parser import Scoop
from profiler import print_prof_data
from codegen.embed_cvxopt import generate

# to make sure atoms get loaded
import atoms

# attach the code generator to the Scoop class
from types import MethodType
Scoop.generate = MethodType(generate, None, Scoop)

# Evaluator.add = MethodType(op_add, None, Evaluator)
# Evaluator.sub = MethodType(op_minus, None, Evaluator)
# Evaluator.mul = MethodType(op_mult, None, Evaluator)
# Evaluator.neg = MethodType(op_neg, None, Evaluator)


