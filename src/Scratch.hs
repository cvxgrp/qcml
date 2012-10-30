module Scratch where
  -- import Control.Applicative
  
  -- goal is to balance code clarity / organization with size
  -- i could write shorter code using template haskell and maybe even monads,
  -- but we're not doing it that way to keep things simple
  
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
    
  data Expr 
    = Expr {
      vexity' :: Curvature,
      sign' :: Sign,
      rows' :: Int,
      cols' :: Int }
    | None
   deriving (Show)
  
  data Parameter = Parameter {
    psign' :: Sign,
    prows' :: Int,
    pcols' :: Int
  } deriving (Show)
  
  class Symbol a where
    vexity :: a -> Curvature
    sign :: a -> Sign
    rows :: a -> Int
    cols :: a -> Int
    
  instance Symbol Expr where
    vexity None = Nonconvex
    vexity x = vexity' x
    sign None = Unknown
    sign x = sign' x
    rows None = 0
    rows x = rows' x
    cols None = 0
    cols x = cols' x
  
  instance Symbol Parameter where
    vexity _ = Affine
    sign = psign'
    rows = prows'
    cols = pcols'
  
  
  -- DCP rules
  applyDCP :: Curvature -> Monotonicity -> Curvature -> Curvature
  applyDCP Convex Increasing Convex = Convex
  applyDCP Convex Decreasing Concave = Convex
  applyDCP Concave Decreasing Convex = Concave
  applyDCP Concave Increasing Concave = Concave
  applyDCP c _ Affine = c
  applyDCP Affine Increasing c = c
  applyDCP Affine Decreasing c = flipVexity c
  applyDCP _ _ _ = Nonconvex
  
  flipVexity :: Curvature -> Curvature
  flipVexity Concave = Convex
  flipVexity Convex = Concave
  flipVexity Affine = Affine
  flipVexity Nonconvex = Nonconvex
  
  
  -- manually applies DCP rules for each atom
  -- atom definitions...

  -- atom properties are implicit in function definitions
  -- this is to avoid using some sort of templating mechanism
  -- to make the code work
  
  square :: Expr -> Expr
  square x = Expr curvature Positive (rows x) (cols x)
    where
      curvature = applyDCP Convex monotonicity (vexity x)
      monotonicity = case (sign x) of
        Positive -> Increasing
        Negative -> Decreasing
        otherwise -> Nonmonotone

  quad_over_lin :: Expr -> Expr -> Expr
  quad_over_lin x y
    | isVector x && isScalar y = Expr c2 Positive 1 1
    | otherwise = None
    where
      c2 = applyDCP c1 Decreasing (vexity y)
      c1 = applyDCP Convex monotonicity (vexity x)
      monotonicity = case (sign x) of
        Positive -> Increasing
        Negative -> Decreasing
        otherwise -> Nonmonotone
        
  constant :: Parameter -> Expr
  constant a = Expr (vexity a) (sign a) (rows a) (cols a)
      
  smult :: Parameter -> Expr -> Expr
  smult a x
    | isScalar a && isVector x = Expr curvature s (rows x) (cols x)
    | otherwise = None
    where
      curvature = applyDCP Affine monotonicity (vexity x)
      monotonicity = case (sign a) of
        Positive -> Increasing
        Negative -> Decreasing
        otherwise -> Nonmonotone
      s = (sign a) <*> (sign x)
  
  mmult :: Parameter -> Expr -> Expr
  mmult a x
    | isMatrix a && isVector x && compatible 
      = Expr curvature s (rows a) (cols x)
    | otherwise = None
    where
      curvature = applyDCP Affine monotonicity (vexity x)
      monotonicity = case (sign a) of
        Positive -> Increasing
        Negative -> Decreasing
        otherwise -> Nonmonotone
      s = (sign a) <*> (sign x)
      compatible = cols a == rows x
  
  
  -- helper functions for guards
  isVector :: (Symbol a) => a -> Bool
  isVector x = (rows x) >= 1 && (cols x) == 1
  
  isScalar :: (Symbol a) => a -> Bool
  isScalar x = (rows x) == 1 && (cols x) == 1
  
  isMatrix :: (Symbol a) => a -> Bool
  isMatrix x = (rows x) >= 1 && (cols x) >= 1
  
  -- sign operations
  (<*>) :: Sign -> Sign -> Sign
  Positive <*> Positive = Positive
  Negative <*> Negative = Positive
  Positive <*> Negative = Negative
  Negative <*> Positive = Negative
  _ <*> _ = Unknown
  
  (<+>) :: Sign -> Sign -> Sign
  Positive <+> Positive = Positive
  Negative <+> Negative = Negative
  _ <+> _ = Unknown

  -- isConvex :: Curvature -> Bool
  -- isConvex Convex = True
  -- isConvex Affine = True
  -- isConvex _ = False
  -- 
  -- isConcave :: Curvature -> Bool
  -- isConcave Concave = True
  -- isConcave Affine = True
  -- isConcave _ = False
  
  -- data Parameter = Parameter Sign
  -- 
  -- data Problem 
  --   = Expr   -- defined as mini SOC problems
  --   | Variable
  --   deriving (Show) 
  -- 
  -- data Tag a
  --   = Tag Curvature [Monotonicity] Sign Size a
  --   deriving (Show)
  -- 
  -- -- data Atom a 
  -- --   = Atom Curvature [Monotonicity] Sign a
  -- --   deriving (Show)
  --   
  -- class Apply f where
  --   (<.>) :: f (a -> b) -> f a -> f b
  -- 
  -- -- DCP rules
  -- instance Apply Tag where
  --   (Tag Convex (Increasing:xs) s sz f) <.> Tag Convex [] _ _ x 
  --     = Tag Convex xs s sz (f x)
  --   (Tag Convex (Decreasing:xs) s sz f) <.> Tag Concave [] _ _ x 
  --     = Tag Convex xs s sz (f x)
  --   (Tag Concave (Decreasing:xs) s sz f) <.> Tag Convex [] _ _ x 
  --     = Tag Concave xs s sz (f x)
  --   (Tag Concave (Increasing:xs) s sz f) <.> Tag Concave [] _ _ x 
  --     = Tag Concave xs s sz (f x)
  --   
  --   (Tag Affine (_:xs) s sz f) <.> Tag Affine [] _ _ x 
  --     = Tag Affine xs s sz (f x)
  --   (Tag c (_:xs) s sz f) <.> Tag Affine [] _ _ x 
  --     = Tag c xs s sz (f x)
  --   (Tag _ (_:xs) s sz f) <.> Tag _ _ _ _ x 
  --     = Tag Nonconvex xs s sz (f x)
  --   
  --  
  -- square :: Sign -> Tag (Problem -> Problem)
  -- square Positive = Tag Convex [Increasing] Positive (Size(1,1)) (\x -> x)
  -- square Negative = Tag Convex [Decreasing] Positive (Size(1,1)) (\x -> x)
  -- square _ = Tag Convex [Nonmonotone] Positive (Size(1,1)) (\x -> x)
  -- 
  -- quad_over_lin :: Sign -> Sign -> Tag (Problem -> Problem -> Problem)
  -- quad_over_lin Positive _ = Tag Convex [Increasing,Decreasing] Positive (Size(1,1)) (\x y -> x)
  -- quad_over_lin Negative _ = Tag Convex [Decreasing,Decreasing] Positive  (Size(1,1))(\x y -> x)
  -- quad_over_lin _ _ = Tag Convex [Nonmonotone, Decreasing] Positive (Size(1,1)) (\x y -> x)