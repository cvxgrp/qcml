module Expression.Expression (positiveSign,
  negativeSign,
  unknownSign,
  positiveParameter,
  negativeParameter,
  parameter,
  variable,
  isValidExpr,
  paramProb,
  module Expression.DCP,
  module Expression.SOCP) where
    
  import Expression.DCP
  import Expression.SOCP
  -- quad over lin x y = minimize t subject to ((y+t)/2, (y-t)/2, x) in SOC3, y in SOC1

  
  -- constant sign functions
  positiveSign = (\_ -> Positive)
  negativeSign = (\_ -> Negative)
  unknownSign = (\_ -> Unknown)
  
  -- XXX: this is the constant atom....
  -- parameter rewriter 
  -- parameter a becomes
  --
  -- minimize t
  -- subject to 
  --    t == a
  --
  paramProb s = (\out _ ->
    -- create a new variable named "t0z0" (or in that form)
    Problem (Just out) [[(out,"1")]] [s] [])
    -- minimize out s.t. out == s
  
  -- constructors (need to take size as arguments)
  positiveParameter :: String -> CVXSymbol
  positiveParameter s = Parameter s (1,1) Affine positiveSign (paramProb s)
  
  negativeParameter :: String -> CVXSymbol
  negativeParameter s = Parameter s (1,1) Affine negativeSign (paramProb s)
  
  parameter :: String -> CVXSymbol
  parameter s = Parameter s (1,1) Affine unknownSign (paramProb s)
  
  variable :: String -> CVXSymbol
  variable s = Variable s (1,1) Affine unknownSign
  
  
  -- check tree validity, easy function to make sure that ParamFunction have 
  -- *parameters* on one side
  isValidExpr :: CVXExpression -> Bool
  isValidExpr (Leaf _) = True
  isValidExpr (Node f args) = 
    let (arguments, parameters) = splitAt (nargs f) args -- assumes parameters follow arguments
        areExprs = map isValidExpr arguments
        areParams = map isParam parameters
    in True && and areExprs && and areParams
  
  isParam :: CVXExpression -> Bool
  isParam (Leaf (Parameter _ _ _ _ _)) = True
  isParam _ = False
  
