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
  ecosMul, ecosAtoms) where
  
  import qualified Data.Map as M
  import Expression.Expression
  -- TODO: in parser, need to check arguments and check parameteric functions
  
  -- list of valid atoms
  ecosAtoms = M.fromList [
    ("square", ecosSquare), 
    ("inv_pos", ecosInvPos),
    ("quad_over_lin", ecosQuadOverLin),
    ("pos", ecosPos),
    ("neg", ecosNeg),
    ("sqrt", ecosSqrt),
    ("geo_mean", ecosGeoMean),
    ("abs", ecosAbs)]
  
  
  -- this might be one way to provide monotonicity
  squareMonotonicity :: [Sign]->[Monotonicity]
  squareMonotonicity [x] = [(monoQuadOverLin [x,Positive])!!0]
  
  monoQuadOverLin :: [Sign] -> [Monotonicity]
  monoQuadOverLin [Positive, _] = [Increasing, Decreasing]
  monoQuadOverLin [Negative, _] = [Decreasing, Decreasing]
  monoQuadOverLin _ = [Nonmonotone, Decreasing]
  
  -- this is how you define atoms (they're defined independently at the moment)
  
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
    ),
    symbolRewrite=(\out inputs ->
        let z0 = VarId $ (label out) ++ "z0"
            z1 = VarId $ (label out) ++ "z1"
        in Problem (Just out) [
          [(out, "0.5"), (z0, "-1")],
          [(out, "-0.5"), (z1, "-1")]
        ] ["-0.5", "-0.5"] [SOC [z0,z1,inputs!!0]]
  )}
  
  -- inv_pos(x) = 1/x for x >= 0
  ecosInvPos :: CVXSymbol
  ecosInvPos = Function {
    name="inv_pos",
    nargs=1,
    symbolVexity=Convex,
    symbolSign=positiveSign,
    monotonicity=(\_ -> [Decreasing]),
    symbolRewrite=(\out inputs ->
        let z0 = VarId $ (label out) ++ "z0"
            z1 = VarId $ (label out) ++ "z1"
            one = VarId $ (label out) ++ "z2"
        in Problem (Just out) [
          [(inputs!!0,"0.5"), (out, "0.5"), (z0, "-1")],
          [(inputs!!0,"0.5"), (out, "-0.5"), (z1, "-1")],
          [(one, "1")]
        ] ["0", "0", "1"] [SOC [z0,z1,one], SOC [inputs!!0]]
  )}
  
  
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
      ),
    symbolRewrite=(\out inputs ->
        let z0 = VarId $ (label out) ++ "z0"
            z1 = VarId $ (label out) ++ "z1"
        in Problem (Just out) [
          [(inputs!!1,"0.5"), (out, "0.5"), (z0, "-1")],
          [(inputs!!1,"0.5"), (out, "-0.5"), (z1, "-1")]
        ] ["0", "0"] [SOC [z0,z1,inputs!!0], SOC [inputs!!1]]
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
    monotonicity=(\x -> [Increasing, Increasing]),
    symbolRewrite=(\out inputs ->
        Problem (Just out) [[(inputs!!0,"1"), (inputs!!1,"1"), (out, "-1")]] ["0"] []
      )
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
    monotonicity=(\_ -> [Increasing, Decreasing]),
    symbolRewrite=(\out inputs ->
        Problem (Just out) [[(inputs!!0,"1"), (inputs!!1,"-1"), (out, "-1")]] ["0"] []
      )
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
    monotonicity=(\_ -> [Decreasing]),
    symbolRewrite=(\out inputs ->
        Problem (Just out) [[(inputs!!0,"-1"), (out, "-1")]] ["0"] []
  )}
  
  -- pos(x) = max(x,0)
  ecosPos :: CVXSymbol
  ecosPos = Function {
    name="pos",
    nargs=1,
    symbolVexity=Convex,
    symbolSign=positiveSign,
    monotonicity=(\_ -> [Increasing]),
    symbolRewrite=(\out inputs ->
        let z0 = VarId $ (label out) ++ "z0"
        in Problem (Just out) [[(out,"1"), (inputs!!0,"-1"), (z0, "-1")]] ["0"] [SOC [out], SOC [z0]]
      )
    }
    
  -- neg(x) = max(-x,0)
  ecosNeg :: CVXSymbol
  ecosNeg = Function {
    name="neg",
    nargs=1,
    symbolVexity=Convex,
    symbolSign=positiveSign,
    monotonicity=(\_ -> [Decreasing]),
    symbolRewrite=(\out inputs ->
        let z0 = VarId $ (label out) ++ "z0"
        in Problem (Just out) [[(out,"1"), (inputs!!0,"1"), (z0, "-1")]] ["0"] [SOC [out], SOC [z0]]
      )
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
      otherwise -> [Nonmonotone]),
    symbolRewrite=(\out inputs ->
      Problem (Just out) [] [] [SOC [out, inputs!!0]]
      )
    }
    
  -- norm2, norm1, norm_inf <-- not implemented since no vectors yet
  -- sqrt(x) = geo_mean(x,1)
  ecosSqrt :: CVXSymbol
  ecosSqrt = Function {
    name="sqrt",
    nargs=1,
    symbolVexity=Concave,
    symbolSign=positiveSign,
    monotonicity=(\_ -> [Increasing]),
    symbolRewrite=(\out inputs ->
        let z0 = VarId $ (label out) ++ "z0"
            z1 = VarId $ (label out) ++ "z1"
        in Problem (Just out) [
          [(inputs!!0,"0.5"), (z0, "-1")],
          [(inputs!!0,"-0.5"), (z1, "-1")]
        ] ["-0.5", "-0.5"] [SOC [z0,z1,out]]
    )
    }
    
  -- geo_mean(x,y) = sqrt(x*y)
  ecosGeoMean :: CVXSymbol
  ecosGeoMean = Function {
    name="geo_mean",
    nargs=2,
    symbolVexity=Concave,
    symbolSign=positiveSign,
    monotonicity=(\_ -> [Increasing, Increasing]),
    symbolRewrite=(\out inputs ->
        let z0 = VarId $ (label out) ++ "z0"
            z1 = VarId $ (label out) ++ "z1"
        in Problem (Just out) [
          [(inputs!!0,"0.5"), (inputs!!1, "0.5"), (z0, "-1")],
          [(inputs!!0,"-0.5"), (inputs!!1, "0.5"), (z1, "-1")]
        ] ["0", "0"] [SOC [z0,z1,out], SOC [inputs!!1]]
    )
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
    ),
    symbolRewrite=(\out inputs ->
        Problem (Just out) [[(inputs!!1,label $ inputs!!0), (out, "-1")]] ["0"] []
      )
  }