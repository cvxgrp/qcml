from scoop import Scoop
from profiler import print_prof_data

# temporarily expose this
from scoop_atoms import Evaluator
from scoop_expression import Constant, Variable, Parameter, POSITIVE, NEGATIVE, UNKNOWN, \
    SCALAR, VECTOR, MATRIX
#from ir.dimension import Dimension, Row, Col, DimSet, Cone

# to make sure atoms get loaded
import atoms


