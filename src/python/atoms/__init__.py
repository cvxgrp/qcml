from scoop.scoop_atoms import Evaluator
from types import MethodType

# atoms from separate files
from add import op_add
from minus import op_minus
from mult import op_mult
from negate import op_neg

from square import scoop_square
    
# attach the atoms to the Evaluator class
Evaluator.add = MethodType(op_add, None, Evaluator)
Evaluator.sub = MethodType(op_minus, None, Evaluator)
Evaluator.mul = MethodType(op_mult, None, Evaluator)
Evaluator.neg = MethodType(op_neg, None, Evaluator)

# build dictionary of methods (matching with identifiers)
macros = \
    {
        'square': scoop_square
    }