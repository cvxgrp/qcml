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

module Expression.Expression (
  Curvature(..), 
  Monotonicity(..), 
  Sign(..),
  Expr(..),
  Param(..),
  Symbol(..),
  Symbolic(..)
) where

  -- import Expression.SOCP
  
  -- data types
  data Curvature
    = Convex
    | Concave
    | Affine
    | Nonconvex
    deriving (Show, Eq)

  data Monotonicity
    = Increasing
    | Decreasing
    | Nonmonotone
    deriving (Show, Eq)

  data Sign
    = Positive
    | Negative
    | Unknown
    deriving (Show, Eq)


  -- TODO: obviously, i could put Expr and Param in same 
  data Expr = Expr String Curvature Sign
  data Param = Param String Sign

  -- container type for Expr and Param symbols
  data Symbol = ESym Expr | PSym Param | CSym Double

  class Symbolic a where
    name :: a -> String
    vexity :: a -> Curvature
    sign :: a -> Sign
    sym :: a -> Symbol

  instance Symbolic Expr where
    name (Expr s _ _) = s
    vexity (Expr _ c _) = c
    sign (Expr _ _ s) = s
    sym x = ESym x

  instance Symbolic Param where
    name (Param s _) = s
    vexity (Param _ _) = Affine
    sign (Param _ s) = s
    sym x = PSym x

  instance Symbolic Double where
    name x = show x
    vexity _ = Affine
    sign x | x >= 0 = Positive
           | x < 0 = Negative
    sym x = CSym x

  instance Symbolic Symbol where
    name (ESym x) = name x
    name (PSym x) = name x
    name (CSym x) = name x
    vexity (ESym x) = vexity x
    vexity (PSym x) = vexity x
    vexity (CSym x) = vexity x
    sign (ESym x) = sign x
    sign (PSym x) = sign x
    sign (CSym x) = sign x
    sym x = x

  
  --instance Symbol Expr where
  --  name (None s) = s
  --  name (Expr v _ _ _) = name v
  --  name (Variable v) = name v
  --  name (Parameter s _ _) = name s
  --  name (Constant x) = display x

  --  rows (None _) = 0
  --  rows (Expr v _ _ _) = rows v
  --  rows (Variable v) = rows v
  --  rows (Parameter p _ Transposed) = cols p
  --  rows (Parameter p _ Diagonal) = rows p
  --  rows (Parameter p _ NoMod) = rows p
  --  rows (Constant _) = 1
    
  --  cols (None _) = 0
  --  cols (Expr v _ _ _) = cols v
  --  cols (Variable v) = cols v
  --  cols (Parameter p _ Transposed) = rows p
  --  cols (Parameter p _ Diagonal) = rows p
  --  cols (Parameter p _ NoMod) = cols p
  --  cols (Constant _) = 1

  ---- type classes to enable vexity inference
  --class DCP a where
  --  vexity :: a -> Curvature
  --  sign :: a -> Sign

  --instance DCP Expr where
  --  vexity (None _) = Nonconvex
  --  vexity (Variable _) = Affine
  --  vexity (Expr _ c _ _) = c
  --  vexity (Parameter _ _ _) = Affine
  --  vexity (Constant _) = Affine
  
  --  sign (None _) = Unknown
  --  sign (Variable _) = Unknown
  --  sign (Expr _ _ s _) = s
  --  sign (Parameter _ s _) = s
  --  sign (Constant x)
  --    | x >= 0 = Positive
  --    | x < 0 = Negative
  --    | otherwise = Unknown

  ---- type class to enable code generation
  --class Rewriteable a where
  --  socp :: a -> SOCP
  --  var :: a -> Var
  --  cones :: a -> ConicSet

  --instance Rewriteable Expr where
  --  socp (Expr v c _ p)
  --    | c == Convex = SOCP Minimize v p
  --    | c == Concave = SOCP Maximize v p
  --    | otherwise = SOCP Find v p
  --  socp (Variable v) = SOCP Find v (ConicSet [] [] [])
  --  -- parameter and constant casting occurs here
  --  socp (Parameter p _ s) = parameterSOCP p s
  --  socp (Constant x) = constantSOCP x
  --  socp (None _) = SOCP Find (Var "0" (1,1)) (ConicSet [] [] [])

  --  var (Expr v _ _ _) = v
  --  var (Variable v) = v
  --  var (Constant x) = obj (constantSOCP x)
  --  var (None _) = Var "0" (1,1)
  --  var (Parameter p _ s) = obj (parameterSOCP p s)

  --  cones (Expr _ _ _ k) = k
  --  cones (Variable _) = ConicSet [] [] []
  --  cones (Parameter p _ s) = constraints (parameterSOCP p s)
  --  cones (Constant x) = constraints (constantSOCP x)
  --  cones (None _) = ConicSet [] [] []

  ---- DCP rules
  --applyDCP :: Curvature -> Monotonicity -> Curvature -> Curvature
  --applyDCP Convex Increasing Convex = Convex
  --applyDCP Convex Decreasing Concave = Convex
  --applyDCP Concave Decreasing Convex = Concave
  --applyDCP Concave Increasing Concave = Concave
  --applyDCP c _ Affine = c
  --applyDCP Affine Increasing c = c
  --applyDCP Affine Decreasing c = flipVexity c
  --applyDCP _ _ _ = Nonconvex

  --flipVexity :: Curvature -> Curvature
  --flipVexity Concave = Convex
  --flipVexity Convex = Concave
  --flipVexity Affine = Affine
  --flipVexity Nonconvex = Nonconvex
  
  ---- constructors for expr, variables, parameter, and none
  --expression :: Var -> Curvature -> Sign -> ConicSet -> Expr
  --expression v c s k = Expr v c s k
  
  --variable :: String -> (Integer, Integer) -> Expr
  --variable name (m,n) = Variable newVar
  --  where newVar = Var name (m,n)
  
  --none :: String -> Expr
  --none s = None s

  --parameter :: String -> Sign -> (Integer, Integer) -> Expr
  --parameter name s (m,n) = Parameter (Param name (m,n)) s NoMod

  --display :: Double -> String
  --display = (map (\x -> if (x=='.') then 'd' else x)).show
  
  ---- helper function to construct SOCP for parameters and constants
  --parameterSOCP :: Param -> ShapeMod -> SOCP
  --parameterSOCP (Param s (m,1)) NoMod = SOCP Find newVar (ConicSet matA vecB [])
  --  where newVar = Var ("p"++s) (m,1)
  --        matA = [Row [(Eye m 1, newVar)]]
  --        vecB = [Vector m (Param s (m,1))]
  --parameterSOCP (Param s (1,m)) Transposed = SOCP Find newVar (ConicSet matA vecB [])
  --  where newVar = Var ("p"++s) (m,1)
  --        matA = [Row [(Eye m 1, newVar)]]
  --        vecB = [VectorT m (Param s (1,m))]
  --parameterSOCP _ _ = SOCP Find (Var "0" (1,1)) (ConicSet [] [] []) -- matrix parameters fail in to this case

  --constantSOCP :: Double -> SOCP
  --constantSOCP x = SOCP Find newVar (ConicSet matA vecB [])
  --  where newVar = Var ("c"++(display x)) (1,1)
  --        matA = [ Row [(Ones 1 1, newVar)] ]
  --        vecB = [Ones 1 x]
