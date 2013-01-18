{--

Copyright (c) 2012-2013, Eric Chu (eytchu@gmail.com)
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met: 

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer. 
2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution. 

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.

The views and conclusions contained in the software and documentation are
those of the authors and should not be interpreted as representing official
policies, either expressed or implied, of the FreeBSD Project.

--}

module Expression.SOCP (
  Sense(..), Var(Var), Param(Param), Symbol(..),
  Row(..), Coeff(..), (<++>),
  ConicSet(..), SOC(..), SOCP(..), VarList(..), coeffs) where

  data SOCP = SOCP {
    sense :: Sense,
    obj :: Var, -- objective is always just a single variable
    constraints :: ConicSet
  } deriving (Show)

  -- problem sense
  data Sense = Maximize | Minimize | Find deriving (Eq, Show)

  -- variables
  -- TODO: to handle constant folding, introduce a "Const" object in addition to Var
  data Var = Var {
    vname:: String,  
    vdims:: (Integer, Integer)
  } deriving (Show)

  data Param = Param {
    pname :: String,
    pdims :: (Integer, Integer)
  } deriving (Show)

  class Symbol a where
    rows :: a -> Integer
    cols :: a -> Integer
    dimensions :: a -> (Integer, Integer)
    name :: a -> String

  instance Symbol Var where
    rows = fst.vdims
    cols = snd.vdims
    dimensions = vdims
    name = vname

  instance Symbol Param where
    rows = fst.pdims
    cols = snd.pdims
    dimensions = pdims
    name = pname
  
  -- for creating coefficients
  -- divided in to constants (Eye, Ones, OnesT)
  -- and parameters (All, Diag, Matrix, Vector)
  --
  -- note that Eye (1) and Ones (1) do the same thing
  -- Eye is a diagonal matrix, Ones is an array
  -- the double stores the *value* of the coefficient
  -- XXX. at the moment, don't need more than *one* value for entire coefficient
  data Coeff = Eye Integer Double   -- eye matrix
      | Ones Integer Double         -- ones vector
      | OnesT Integer Double        -- ones' row vector
      | Diag Integer Param          -- diagonal matrix (replicated parameter)
      | Matrix Param            -- generic matrix (matrices are the "largest" shape, so we don't need an extra Integer to tell us how it's sized--the Param itself is sized properly)
      | MatrixT Param           -- matrix transpose
      | Vector Integer Param        -- generic vector
      | VectorT Integer Param       -- vector transpose
      -- should add transpose here
      deriving (Show)
  
  data ConicSet = ConicSet {
    matrixA :: [Row],
    vectorB :: [Coeff],
    conesK :: [SOC]
  } deriving (Show)

  (<++>) :: ConicSet -> ConicSet -> ConicSet
  x <++> y = ConicSet (matrixA x ++ matrixA y) (vectorB x ++ vectorB y) (conesK x ++ conesK y)


  -- differentiate between SOC and elementwise SOC
  -- SOC [x,y,z] means norm([y,x]) <= z
  -- SOCelem [x,y,z] means norms([x y]')' <= z
  -- note that SOC [x] and SOCelem [x] both mean x >= 0
  data SOC = SOC { vars :: [Var] } 
    | SOCelem { vars :: [Var] }
    deriving (Show)

  -- TODO: we can make a "concat" row type?
  -- TODO: we can include the row height in here
  data Row = Row { elems :: [(Coeff,Var)] } deriving (Show)

  -- type class for accessing variables
  class VarList a where
    variables :: a -> [Var]
    varnames :: a -> [String]

  instance VarList SOC where
    variables = vars
    varnames x = map vname (vars x)

  instance VarList Row where
    variables (Row x) = map snd x
    varnames (Row x) = map (vname.snd) x

  coeffs :: Row -> [Coeff]
  coeffs (Row x) = map fst x