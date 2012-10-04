ECOS Front End
==============

This project contains a front end for the *embedded conic solver* (ECOS) for *second-order cone programs* (SOCP).

The `src` folder contains the Haskell source to parse the input.

  Running `make` in this folder will produce three binaries: `main`, `test`, 
  and `probtocvx`. At the moment, only `probtocvx` does anything. The other 
  two binaries are in various states of neglect. The `probtocvx` binary takes
  a text file as input and outputs the CVX representation of the problem (if
  the problem is signed DCP compliant).

The `doc` folder contains the accompanying documentation. 
