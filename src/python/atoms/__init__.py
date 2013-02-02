from scoop.scoop_atoms import Evaluator
from types import MethodType
from add import op_add
    
# attach the atoms to the Evaluator class
Evaluator.add = MethodType(op_add, None, Evaluator)