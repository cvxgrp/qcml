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
    ("abs", ecosAbs),
    ("max", ecosMax),
    ("min", ecosMin)]
  
  
  -- -- this might be one way to provide monotonicity
  -- squareMonotonicity :: [Sign]->[Monotonicity]
  -- squareMonotonicity [x] = [(monoQuadOverLin [x,Positive])!!0]
  -- 
  -- monoQuadOverLin :: [Sign] -> [Monotonicity]
  -- monoQuadOverLin [Positive, _] = [Increasing, Decreasing]
  -- monoQuadOverLin [Negative, _] = [Decreasing, Decreasing]
  -- monoQuadOverLin _ = [Nonmonotone, Decreasing]
  
  -- this is how you define atoms (they're defined independently at the moment)
  
  -- square(x) = x^2
  ecosSquare :: CVXSymbol
  ecosSquare = Atom {
    name="square",
    nargs=1,
    nparams=0,
    symbolVexity=Convex,
    symbolSign=positiveSign,
    monotonicity=(\x -> case(x) of
      [Positive] -> [Increasing]
      [Negative] -> [Decreasing]
      otherwise -> [Nonmonotone]
    ),
    symbolRewrite=(\out inputs ->
        let z0 = VarId (label out ++ "z0") 1 1
            z1 = VarId (label out ++ "z1") 1 1
        in Problem (Just out) [
          [(out, "0.5"), (z0, "-1")],
          [(out, "-0.5"), (z1, "-1")]
        ] ["-0.5", "-0.5"] [SOC [z0,z1,inputs!!0]]
  )}
  
  -- inv_pos(x) = 1/x for x >= 0
  ecosInvPos :: CVXSymbol
  ecosInvPos = Atom {
    name="inv_pos",
    nargs=1,
    nparams=0,
    symbolVexity=Convex,
    symbolSign=positiveSign,
    monotonicity=(\_ -> [Decreasing]),
    symbolRewrite=(\out inputs ->
        let z0 = VarId (label out ++ "z0") 1 1
            z1 = VarId (label out ++ "z1") 1 1
            one = VarId (label out ++ "z2") 1 1
        in Problem (Just out) [
          [(inputs!!0,"0.5"), (out, "0.5"), (z0, "-1")],
          [(inputs!!0,"0.5"), (out, "-0.5"), (z1, "-1")],
          [(one, "1")]
        ] ["0", "0", "1"] [SOC [z0,z1,one], SOC [inputs!!0]]
  )}
  
  
  -- quad_over_lin(x) = x^2/y
  ecosQuadOverLin :: CVXSymbol
  ecosQuadOverLin = Atom {
    name="quad_over_lin",
    nargs=2,
    nparams=0,
    symbolVexity=Convex,
    symbolSign=positiveSign,
    monotonicity=(\x -> case (x) of
      [Positive, _] -> [Increasing, Decreasing]
      [Negative, _] -> [Decreasing, Decreasing]
      otherwise -> [Nonmonotone,Decreasing]
      ),
    symbolRewrite=(\out inputs ->
        let z0 = VarId (label out ++ "z0") 1 1 
            z1 = VarId (label out ++ "z1") 1 1
        in Problem (Just out) [
          [(inputs!!1,"0.5"), (out, "0.5"), (z0, "-1")],
          [(inputs!!1,"0.5"), (out, "-0.5"), (z1, "-1")]
        ] ["0", "0"] [SOC [z0,z1,inputs!!0], SOC [inputs!!1]]
    )}
  
  -- plus(x,y) = x + y
  ecosPlus :: CVXSymbol
  ecosPlus = Atom {
    name="plus",
    nargs=2,
    nparams=0,
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
  ecosMinus = Atom {
    name="minus",
    nargs=2,
    nparams=0,
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
  ecosNegate = Atom {
    name="negate",
    nargs=1,
    nparams=0,
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
  ecosPos = Atom {
    name="pos",
    nargs=1,
    nparams=0,
    symbolVexity=Convex,
    symbolSign=positiveSign,
    monotonicity=(\_ -> [Increasing]),
    symbolRewrite=(\out inputs ->
        let z0 = VarId (label out ++ "z0") 1 1
        in Problem (Just out) [[(out,"1"), (inputs!!0,"-1"), (z0, "-1")]] ["0"] [SOC [out], SOC [z0]]
      )
    }
    
  -- neg(x) = max(-x,0)
  ecosNeg :: CVXSymbol
  ecosNeg = Atom {
    name="neg",
    nargs=1,
    nparams=0,
    symbolVexity=Convex,
    symbolSign=positiveSign,
    monotonicity=(\_ -> [Decreasing]),
    symbolRewrite=(\out inputs ->
        let z0 = VarId (label out ++ "z0") 1 1
        in Problem (Just out) [[(out,"1"), (inputs!!0,"1"), (z0, "-1")]] ["0"] [SOC [out], SOC [z0]]
      )
    }
    
  -- abs(x) = |x|
  ecosAbs :: CVXSymbol
  ecosAbs = Atom {
    name="abs",
    nargs=1,
    nparams=0,
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
  ecosSqrt = Atom {
    name="sqrt",
    nargs=1,
    nparams=0,
    symbolVexity=Concave,
    symbolSign=positiveSign,
    monotonicity=(\_ -> [Increasing]),
    symbolRewrite=(\out inputs ->
        let z0 = VarId (label out ++ "z0") 1 1
            z1 = VarId (label out ++ "z1") 1 1
        in Problem (Just out) [
          [(inputs!!0,"0.5"), (z0, "-1")],
          [(inputs!!0,"-0.5"), (z1, "-1")]
        ] ["-0.5", "-0.5"] [SOC [z0,z1,out]]
    )
    }
    
  -- geo_mean(x,y) = sqrt(x*y)
  ecosGeoMean :: CVXSymbol
  ecosGeoMean = Atom {
    name="geo_mean",
    nargs=2,
    nparams=0,
    symbolVexity=Concave,
    symbolSign=positiveSign,
    monotonicity=(\_ -> [Increasing, Increasing]),
    symbolRewrite=(\out inputs ->
        let z0 = VarId (label out ++ "z0") 1 1
            z1 = VarId (label out ++ "z1") 1 1
            -- negOut = -out; gives the right solution to maximization problem
            -- negOut = VarId $ (label out) ++ "z2"  
        in Problem (Just out) [
          [(inputs!!0,"0.5"), (inputs!!1, "0.5"), (z0, "-1")],
          [(inputs!!0,"-0.5"), (inputs!!1, "0.5"), (z1, "-1")]
        ] ["0", "0"] [SOC [z0,z1,out], SOC [inputs!!1]]
    )
    }
    
  -- pow_rat(x,p,q) <-- not implemented for the moment
  -- sum_largest(x,k) <-- also not implemented (uses LP dual)
  
  -- max(x,y)
  ecosMax:: CVXSymbol
  ecosMax = Atom {
    name="max",
    nargs=2,
    nparams=0,
    symbolVexity=Convex,
    symbolSign = (\x -> case(x) of
      [Positive,_] -> Positive
      [_,Positive] -> Positive
      [Negative,Negative] -> Negative
      otherwise -> Unknown
    ),
    monotonicity=(\_ -> [Increasing, Increasing]),
    symbolRewrite=(\out inputs ->
        let z0 = VarId (label out ++ "z0") 1 1
            z1 = VarId (label out ++ "z1") 1 1
        in Problem (Just out) [
          [(out,"1"), (inputs!!0,"-1"), (z0, "-1")],
          [(out,"1"), (inputs!!1,"-1"), (z1, "-1")]
        ] ["0", "0"] [SOC [z0], SOC [z1]]
    )
    }
    
  -- min(x,y)
  ecosMin :: CVXSymbol
  ecosMin = Atom {
    name="min",
    nargs=2,
    nparams=0,
    symbolVexity=Concave,
    symbolSign = (\x -> case(x) of
      [Negative,_] -> Negative
      [_,Negative] -> Negative
      [Positive,Positive] -> Positive
      otherwise -> Unknown
    ),
    monotonicity=(\_ -> [Increasing, Increasing]),
    symbolRewrite=(\out inputs ->
        let z0 = VarId (label out ++ "z0") 1 1
            z1 = VarId (label out ++ "z1") 1 1
            -- negOut = -out; gives the right solution to maximization problem
            negOut = VarId $ (label out) ++ "z2"
        in Problem (Just out) [
          [(out,"-1"), (inputs!!0,"1"), (z0, "-1")],
          [(out,"-1"), (inputs!!1,"1"), (z1, "-1")]
          --[(out, "1"), (out,"1")]
        ] ["0", "0"] [SOC [z0], SOC [z1]]
    )
    }
    
  -- this is how you define *parameterized* atoms

  -- mul(x; a) = a*x
  ecosMul :: CVXSymbol
  ecosMul = Atom {
    name="multiply",
    nargs=1,
    nparams=1,
    symbolVexity=Affine,
    symbolSign=(\x -> case (x) of
      [Positive, Positive] -> Positive
      [Negative, Negative] -> Positive
      [Positive, Negative] -> Negative
      [Negative, Positive] -> Negative
      otherwise -> Unknown
    ),
    monotonicity=(\x -> case (x) of
      [_, Positive] -> [Increasing, Nonmonotone]
      [_, Negative] -> [Decreasing, Nonmonotone]
      otherwise -> [Nonmonotone, Nonmonotone]
    ),
    symbolRewrite=(\out inputs ->
        Problem (Just out) [[(inputs!!0,label $ inputs!!1), (out, "-1")]] ["0"] []
      )
  }