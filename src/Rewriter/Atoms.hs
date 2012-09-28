module Rewriter.Atoms (ecosSquare,
  ecosInvPos,
  ecosQuadOverLin, 
  ecosPlus, 
  ecosMinus,
  ecosNegate,
  ecosPos,
  ecosNeg,
  ecosAbs,
  ecosSqrt,
  ecosGeoMean,
  ecosMul) where
  
  import Expression.Expression
  -- TODO: in parser, need to check arguments and check parameteric functions
  
  
  -- this might be one way to provide monotonicity
  squareMonotonicity :: [Sign]->[Monotonicity]
  squareMonotonicity [x] = [(monoQuadOverLin [x,Positive])!!0]
  
  monoQuadOverLin :: [Sign] -> [Monotonicity]
  monoQuadOverLin [Positive, _] = [Increasing, Decreasing]
  monoQuadOverLin [Negative, _] = [Decreasing, Decreasing]
  monoQuadOverLin _ = [Nonmonotone, Decreasing]
  
  -- this is how you define atoms
  
  -- square(x) = x^2
  ecosSquare :: CVXSymbol
  ecosSquare = Function {
    name="square",
    nargs=1,
    symbolVexity=Convex,
    symbolSign=positiveSign,
    monotonicity=(\x -> case(x) of
      [Positive] -> [Increasing]
      [Negative] -> [Decreasing]
      otherwise -> [Nonmonotone]
    )}
  
  -- inv_pos(x) = 1/x for x >= 0
  ecosInvPos :: CVXSymbol
  ecosInvPos = Function {
    name="inv_pos",
    nargs=1,
    symbolVexity=Convex,
    symbolSign=positiveSign,
    monotonicity=(\_ -> [Decreasing])
    }
  
  
  -- quad_over_lin(x) = x^2/y
  ecosQuadOverLin :: CVXSymbol
  ecosQuadOverLin = Function {
    name="quad_over_lin",
    nargs=2,
    symbolVexity=Convex,
    symbolSign=positiveSign,
    monotonicity=(\x -> case (x) of
      [Positive, _] -> [Increasing, Decreasing]
      [Negative, _] -> [Decreasing, Decreasing]
      otherwise -> [Nonmonotone,Decreasing]
      )}
  
  -- plus(x,y) = x + y
  ecosPlus :: CVXSymbol
  ecosPlus = Function {
    name="plus",
    nargs=2,
    symbolVexity=Affine,
    symbolSign=(\x -> case(x) of
      [Positive,Positive] -> Positive
      [Negative,Negative] -> Negative
      otherwise -> Unknown
    ),
    monotonicity=(\x -> [Increasing, Increasing])
    }
    
  -- minus(x,y) = x - y
  ecosMinus :: CVXSymbol
  ecosMinus = Function {
    name="minus",
    nargs=2,
    symbolVexity=Affine,
    symbolSign=(\x -> case(x) of
      [Positive,Negative] -> Positive
      [Negative,Positive] -> Negative
      otherwise -> Unknown
    ),
    monotonicity=(\_ -> [Increasing, Decreasing])
    }
  
  -- negate(x) = -x
  ecosNegate :: CVXSymbol
  ecosNegate = Function {
    name="negate",
    nargs=1,
    symbolVexity=Affine,
    symbolSign=(\x -> case(x) of
      [Positive] -> Negative
      [Negative] -> Positive
      otherwise -> Unknown
    ),
    monotonicity=(\_ -> [Decreasing])
    }
  
  -- pos(x) = max(x,0)
  ecosPos :: CVXSymbol
  ecosPos = Function {
    name="pos",
    nargs=1,
    symbolVexity=Convex,
    symbolSign=positiveSign,
    monotonicity=(\_ -> [Increasing])
    }
    
  -- neg(x) = max(-x,0)
  ecosNeg :: CVXSymbol
  ecosNeg = Function {
    name="neg",
    nargs=1,
    symbolVexity=Convex,
    symbolSign=positiveSign,
    monotonicity=(\_ -> [Decreasing])
    }
    
  -- abs(x) = |x|
  ecosAbs :: CVXSymbol
  ecosAbs = Function {
    name="abs",
    nargs=1,
    symbolVexity=Convex,
    symbolSign=positiveSign,
    monotonicity=(\x -> case(x) of
      [Positive] -> [Increasing]
      [Negative] -> [Decreasing]
      otherwise -> [Nonmonotone]
    )}
    
  -- norm2, norm1, norm_inf <-- not implemented since no vectors yet
  -- sqrt(x) = geo_mean(x,1)
  ecosSqrt :: CVXSymbol
  ecosSqrt = Function {
    name="sqrt",
    nargs=1,
    symbolVexity=Concave,
    symbolSign=positiveSign,
    monotonicity=(\_ -> [Increasing])
    }
    
  -- geo_mean(x,y) = sqrt(x*y)
  ecosGeoMean :: CVXSymbol
  ecosGeoMean = Function {
    name="geo_mean",
    nargs=2,
    symbolVexity=Concave,
    symbolSign=positiveSign,
    monotonicity=(\_ -> [Increasing, Increasing])
    }
    
  -- pow_rat(x,p,q) <-- not implemented for the moment
  -- sum_largest(x,k) <-- also not implemented (uses LP dual)
  -- max(x), min(x) <-- also not implemented, since no vectors
  
  -- this is how you define *parameterized* atoms
  -- hmm, no different... only restriction happens when we *construct* the tree...
  -- alternative is to have ParamFunction take a Parameter as a type constructor argument
  -- not sure if that will make a difference during code generation
    
  -- mul(x; a) = a*x
  ecosMul :: CVXSymbol
  ecosMul = ParamFunction {
    name="multiply",
    nargs=2,
    symbolVexity=Affine,
    symbolSign=(\x -> case (x) of
      [Positive, Positive] -> Positive
      [Negative, Negative] -> Positive
      [Positive, Negative] -> Negative
      [Negative, Positive] -> Negative
      otherwise -> Unknown
    ),
    monotonicity=(\x -> case (x) of
      [Positive, _] -> [Nonmonotone, Increasing]
      [Negative, _] -> [Nonmonotone, Decreasing]
      otherwise -> [Nonmonotone, Nonmonotone]
    )
  }