module Atoms.SCOOP_Square(scoop_square) where
  import Atoms.Common
  import qualified Data.Map as M
  import Control.Monad.State
  --import CodeGenerator.Common hiding (find)
  --import CodeGenerator.CVX


  scoop_constant :: Double -> Expression
  scoop_constant x = do
    t <- newVar 1

    find t
    subjectTo
    (Ones 1 1) .* t .== Ones 1 x

    return $ MyConstant t x

  -- everything else is being done "under the hood"
  scoop_square :: (ShapedVar a) => a -> Expression
  scoop_square x = do
    case(sign'' x) of
      Positive -> positive <&> increasing x   -- TODO: fix this syntax... <&> is actually kind of special (not just >> / sequence of ops)
      Negative -> positive <&> decreasing x
      otherwise -> positive <&> nonmonotone x

    let m = rows'' x
    t <- newVar m
    z0 <- newVar m
    z1 <- newVar m

    -- definition
    (v,s) <- minimize t
    subjectTo   -- since this is a no-op, haskell should be lazy and optimize it out
    [z0, z1, var'' x] `isIn` SOCelem
    (Eye m 0.5).*t .+ (Eye m (-1)).*z0 .== Ones m (-0.5)
    (Eye m (-0.5)).*t .+ (Eye m (-1)).*z1 .== Ones m (-0.5)

    return $ MyExpr t v s -- should read MyExpr t myVexity mySign

  scoop_negate :: (ShapedVar a) => a -> Expression
  scoop_negate x = do
    case(sign'' x) of
      Positive -> negative <&> decreasing x
      Negative -> positive <&> decreasing x
      otherwise -> unknown <&> decreasing x

    let m = rows'' x
    t <- newVar m
    (v,s) <- find t
    subjectTo
    (Eye m (-1)).*(var'' x) .+ (Eye m (-1)).*t .== Ones m 0

    return $ MyExpr t v s

  scoop_plus :: (ShapedVar a, ShapedVar b) => a -> b -> Expression
  scoop_plus x y = do
    -- doesn't yet check compatible sizes (or handle special cases of scalar + vector)
    case(sign'' x, sign'' y) of
      (Positive, Positive) -> positive <&> increasing x <&> increasing y
      (Negative, Negative) -> negative <&> increasing x <&> increasing y
      otherwise -> unknown <&> increasing x <&> increasing y
    
    let m = rows'' x
    t <- newVar m
    (v,s) <- find t
    subjectTo
    (Eye m 1).* var'' x .+ (Eye m 1).*var'' y .+ (Eye m (-1)).*t .== Ones m 0

    return $ MyExpr t v s


  -- atoms will be bound by type
  -- will create container in parser so params, constants, and vars/exprs are in same container
  -- but the operations will "unpack" them

  scoop_mult' :: (Paramed a, ShapedVar b) => a -> b -> Expression
  scoop_mult' x y = fail "well, you got this far"

  --scoop_mult (None s) _ _ = None s
  --scoop_mult _ (None s) _ = None s
  --scoop_mult (Parameter p psgn shape) x s
  --  | (pm == 1) && (pn == 1) && isVector x = expression newVar curvature sgn prog
  --  | (pm >= 1) && (pn >= 1) && isVector x && compatible = expression newVar curvature sgn prog
  --  | otherwise = none $ "mult: size of " ++ (name p) ++ " and " ++ (name x) ++ " don't match"
  --  where
  --    curvature = applyDCP Affine monotonicity (vexity x)
  --    monotonicity = case (psgn) of
  --      Positive -> Increasing
  --      Negative -> Decreasing
  --      otherwise -> Nonmonotone
  --    sgn = psgn |*| (sign x)
  --    compatible = pn == rows x
  --    prog = (ConicSet matA vecB []) <++> (cones x)
  --    (pm,pn)
  --      | shape == NoMod = (rows p, cols p)
  --      | shape == Transposed = (cols p, rows p)
  --      | shape == Diagonal = (rows p, rows p)
  --    (m,n)
  --      | pm == 1 && pn == 1 = (rows x, cols x)
  --      | otherwise = (pm, cols x)
  --    newVar = Var ("t"++s) (m, n)
  --    matA
  --      | pm == 1 && pn == 1 = [ Row [(Diag m p, var x), (Eye m (-1), newVar)] ]
  --      | shape == Diagonal = [ Row [(Diag m p, var x), (Eye m (-1), newVar)] ]
  --      | shape == Transposed = [ Row [(MatrixT p, var x), (Eye m (-1), newVar)] ]
  --      | shape == NoMod = [ Row [(Matrix p, var x), (Eye m (-1), newVar)] ]
  --    vecB = [Ones m 0]
  --scoop_mult _ _ _ = None "mult: lhs ought to be parameter"

  symbolTable' :: M.Map String Expr
  symbolTable' = M.empty

  testagain :: Expression
  testagain = do
    x <- newVar 1
    a <- scoop_square x
    c <- scoop_negate a

    t <- scoop_square x  
    -- scoop_mult' (3.0::Double) t
    -- rewriting is carried in the context
    -- so arguments must be rewritten before being called
    -- this means only vars and expr can be passed to atoms
    y <- scoop_negate t
    scoop_plus y c

  genP = execState testagain initState

  getProb :: ExpressionState -> SOCP
  getProb (_,_,_,x) = x

  --testme = cvxgen (Codegen (getProb genP) symbolTable')

  --class Symbol' a where
  --  rows' :: a -> Integer
  --  rows' = rows' . var'
  --  cols' :: a -> Integer
  --  cols' = cols' . var'
  --  dimensions' :: a -> (Integer, Integer)
  --  dimensions' x = (rows' x, cols' x)
  --  name' :: a -> String
  --  name' = name' . var'
  --  var' :: a -> Var
