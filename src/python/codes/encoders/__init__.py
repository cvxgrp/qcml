"""
If you want to add other languages for printing matrices, do it in this
folder. You must define a lookup table for the different coefficient classes,
then call 'create_encoder' found in encoder.py.

These are the coefficient classes you must define a print operation for:

TODO
"""
from python_encoder import toPython
from c_encoder import toC
from matlab_encoder import toMatlab