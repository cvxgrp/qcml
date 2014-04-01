""" This contains the basic codes that we need for code generation.

    These include the coefficients which need to have code generated and
    other types of codes (e.g., for loops) that are not yet implemented.

    The encoders for writing the code are also found here.

    ECHU: Note to AJ, in the future, structured linear algebra objects live
    in this folder. The encoder figures out how to convert the objects into
    the appropriate code.
"""
from . code import Just, LoopRows, LoopCols, LoopOver, Range, Repeat, \
    Assign, NNZ
from . coefficients.coefficient import ConstantCoeff, OnesCoeff, \
    NegateCoeff, AddCoeff, MulCoeff, EyeCoeff, TransposeCoeff, \
    ParameterCoeff, ScalarParameterCoeff, SliceCoeff
