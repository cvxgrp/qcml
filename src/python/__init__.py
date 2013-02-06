from scoop import Scoop
from profiler import print_prof_data

# temporarily expose this
from scoop_atoms import Evaluator, Cone, Row
from scoop_expression import Constant, Variable, Parameter, POSITIVE, NEGATIVE, UNKNOWN, \
    SCALAR, VECTOR, MATRIX
from ir.dimension import Dimension, Row, Col

# to make sure atoms get loaded
import atoms


