"""
If you want to add other languages for printing matrices, do it in this
folder. You must define a lookup table for the different coefficient classes,
then call 'create_encoder' found in encoder.py.

These are the coefficient classes you must define a print operation for:

TODO list out the coefficient classes
TODO there are actually multiple kinds of encoders depending on data
    structures and language

    For instance, if we are writing a Python encoder, it's possible to use
    either numpy or scipy data structures (for the input *and* for the
    output). Furthermore, if the sparsity pattern and dimensions are fixed
    there are optimizations that can be done to the code.

    The current plan is to 'compile' and 'codegen' the most generic code; it
    uses the coordinate format to stuff the matrices and compresses it at
    the end.

    It will do this *regardless* of the data supplied; however, if the
    dimensions are supplied, then static data structures can be used for
    *some* things. If the sparsity patterns are also supplied, then the
    mapping into CSR can be generated *automatically*.

    The code generator *will not* do this unless certain 'compiler flags'
    are set.

    I think that's the "future."
"""
from . python_encoder import toPython
from . c_encoder import toC
from . matlab_encoder import toMatlab
