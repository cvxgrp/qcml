module Rewriter.Atoms (evalBinaryOp, evalUnaryOp) where
  import qualified Data.Map as M
  import Expression.Expression
  
  type CVXExpression = Expression Atom Node
  
  data Atom = Plus
    | Sub
    | Mul
    | QuadOverLin
    | Square
    | InvPos
    | Abs
    deriving (Show, Eq)
    
  data Node = Parameter String | Variable String deriving (Show, Eq)

  instance Vexity CVXExpression where
    vexity Leaf _ = Affine
    vexity Binary op left right = compose op left right
    vexity Unary op rest = compose op left
    vexity _ = Nonconvex
    
    sign (Leaf _) = Unknown
    sign _ = Unknown
    
  instance Function Atom where
    monotonicity Plus [x,y]
    
    monotonicity _ = []
  
  -- instance Vexity Atom where
  --   vexity Plus = invokeCompositionRule 
  --   vexity Sub
  --   vexity Mul
  --   vexity QuadOverLin
  --   vexity Square
  --   vexity InvPos
  --   vexity Abs
  --   
  --   sign 

  -- -- data types for annotating the expression tree
  -- data Vexity = Convex
  --   | Concave
  --   | Affine
  --   deriving(Show, Eq)
  -- 
  -- data Sign = Positive
  --   | Negative
  --   deriving (Show, Eq)
  -- 
  -- data Expr = Expr { sign' :: Maybe Sign,
  --                    vexity' :: Maybe Vexity }
  --           | Constant Double
  --           deriving (Show)
  -- 
  -- vexity :: Expr -> Maybe Vexity
  -- vexity (Constant _) = Just Affine
  -- vexity s = vexity' s
  -- 
  -- sign :: Expr -> Maybe Sign
  -- sign (Constant d)
  --   | d >= 0 = Just Positive
  --   | otherwise = Just Negative
  -- sign s = sign' s
  --           
  -- -- utility functions for manipulating expressions
  -- constExpr :: Double -> Expr
  -- constExpr d = Constant d
  -- 
  -- convexExpr :: Sign -> Expr
  -- convexExpr s = Expr (Just s) (Just Convex)
  -- 
  -- concaveExpr :: Sign -> Expr
  -- concaveExpr s = Expr (Just s) (Just Concave)
  -- 
  -- affineExpr :: Sign -> Expr
  -- affineExpr s = Expr (Just s) (Just Affine)
  -- 
  -- negateExpr :: Expr -> Expr
  -- negateExpr (Constant d) = constExpr $ negate d
  -- negateExpr (Expr (Just Positive) v) = Expr (Just Negative) v
  -- negateExpr (Expr (Just Negative) v) = Expr (Just Positive) v
  -- 
  -- isConvex :: Expr -> Bool
  -- isConvex expr = not $ (vexity expr) == Just Concave
  -- 
  -- isConcave :: Expr -> Bool
  -- isConcave expr = not $ (vexity expr) == Just Convex
  -- 
  -- isAffine :: Expr -> Bool
  -- isAffine expr = isConvex expr && isConcave expr
  -- 
  -- isPositive :: Expr -> Bool
  -- isPositive expr = (sign expr) == Just Positive
  -- 
  -- isNegative :: Expr -> Bool
  -- isNegative expr = not (isPositive expr)
  -- 
  -- isConstant :: Expr -> Bool
  -- isConstant expr = case (expr) of
  --   Constant _ -> True
  --   otherwise -> False
    
  -- Atom function vexity reasoning
  
  -- ecos_quad_over_lin :: Expr -> Expr -> Expr
  -- ecos_quad_over_lin x y
  --   | isPositive y && isAffine x && isConcave y = convexExpr Positive
  --   | isNegative y && isAffine x && isConvex y = concaveExpr Negative -- this could be wrong...
  --   | isPositive x && isPositive y && isConvex x && isConcave y = convexExpr Positive
  --   | isPositive x && isNegative y && isConvex x && isConcave y = concaveExpr Negative
  --   | otherwise = Expr (Nothing) (Nothing)
  -- 
  -- ecos_square :: Expr -> Expr
  -- ecos_square x = (\x -> ecos_quad_over_lin x (constExpr 1)) x
  -- 
  -- ecos_inv :: Expr -> Expr
  -- ecos_inv x = ecos_quad_over_lin (constExpr 1) x
  -- 
  -- ecos_abs :: Expr -> Expr
  -- ecos_abs x
  --   | isPositive x && isConvex x = convexExpr Positive
  --   | isAffine x = convexExpr Positive
  --   | otherwise = Expr (Just Positive) (Nothing)
  -- 
  -- ecos_plus :: Expr -> Expr -> Expr
  -- ecos_plus x y
  --   | isAffine x && isAffine y = Expr (add_sign x y) (Just Affine)
  --   | isConvex x && isConvex y = Expr (add_sign x y) (Just Convex)
  --   | isConcave x && isConcave y = Expr (add_sign x y) (Just Concave)
  --   | otherwise = Expr (add_sign x y) Nothing
  -- 
  -- ecos_sub :: Expr -> Expr -> Expr
  -- ecos_sub x y = ecos_plus x (negateExpr y)
  -- 
  -- ecos_mul :: Expr -> Expr -> Expr
  -- ecos_mul x y 
  --   | isConstant x && isAffine y = Expr (mul_sign x y) (Just Affine)
  --   | isConstant y && isAffine x = Expr (mul_sign x y) (Just Affine)
  --   | isConstant x && isConvex y = case (sign x) of
  --       Just Positive -> Expr (mul_sign x y) (Just Convex)
  --       Just Negative -> Expr (mul_sign x y) (Just Concave)
  --       otherwise -> Expr (Nothing) (Nothing)
  --   | isConstant x && isConcave y = case (sign x) of
  --       Just Positive -> Expr (mul_sign x y) (Just Concave)
  --       Just Negative -> Expr (mul_sign x y) (Just Convex)
  --       otherwise -> Expr (Nothing) (Nothing)
  --   | isConstant y && isConvex x = case (sign y) of
  --       Just Positive -> Expr (mul_sign x y) (Just Convex)
  --       Just Negative -> Expr (mul_sign x y) (Just Concave)
  --       otherwise -> Expr (Nothing) (Nothing)
  --   | isConstant y && isConcave x = case (sign y) of
  --       Just Positive -> Expr (mul_sign x y) (Just Concave)
  --       Just Negative -> Expr (mul_sign x y) (Just Convex)
  --       otherwise -> Expr (Nothing) (Nothing)
  --   | otherwise = Expr (Nothing) (Nothing)
  --   
  -- add_sign :: Expr -> Expr -> Maybe Sign
  -- add_sign x y = case (sign x, sign y) of
  --   (Just Positive, Just Positive) -> Just Positive
  --   (Just Negative, Just Negative) -> Just Negative
  --   otherwise -> Nothing
  -- 
  -- mul_sign :: Expr -> Expr -> Maybe Sign
  -- mul_sign x y = case (sign x, sign y) of
  --   (Just Positive, Just Positive) -> Just Positive
  --   (Just Negative, Just Negative) -> Just Positive
  --   (Just Positive, Just Negative) -> Just Negative
  --   (Just Negative, Just Positive) -> Just Negative
  --   otherwise -> Nothing
  -- 
  -- -- evaluators
  -- 
  -- evalBinaryOp :: String -> [Expr] -> [Expr]
  -- evalBinaryOp s (x:y:rest) = case (M.lookup s $ binaryOps) of
  --   Just f -> (f x y):rest
  --   Nothing -> (x:y:rest)
  -- evalBinaryOp s expr = expr
  -- 
  -- 
  -- evalUnaryOp :: String -> [Expr] -> [Expr]
  -- evalUnaryOp s (x:rest) = case (M.lookup s $ unaryOps) of
  --   Just f -> (f x):rest
  --   Nothing -> (x:rest)
  -- evalUnaryOp s expr = expr
  -- 
  -- -- list of atoms
  -- 
  -- binaryOps = M.fromList [
  --   ("plus", ecos_plus),
  --   ("sub", ecos_sub),
  --   ("mul", ecos_mul),
  --   ("quad_over_lin", ecos_quad_over_lin)
  --   ]
  -- 
  -- unaryOps = M.fromList [
  --   ("square", ecos_square),
  --   ("inv_pos", ecos_inv),
  --   ("abs", ecos_abs)
  --   ]