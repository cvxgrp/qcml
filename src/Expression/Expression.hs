module Expression.Expression (positiveSign,
  negativeSign,
  unknownSign,
  positiveParameter,
  negativeParameter,
  parameter,
  variable,
  isValidExpr,
  module Expression.DCP,
  module Expression.SOCP) where
    
  import Expression.DCP
  import Expression.SOCP
  -- quad over lin x y = minimize t subject to ((y+t)/2, (y-t)/2, x) in SOC3, y in SOC1

  
  -- constant sign functions
  positiveSign = (\_ -> Positive)
  negativeSign = (\_ -> Negative)
  unknownSign = (\_ -> Unknown)
  
  sizeFunc x = (\_ -> x)
  -- XXX: this is the constant atom....
  -- parameter rewriter 
  -- parameter a becomes
  --
  -- minimize t
  -- subject to 
  --    t == a
  --
  paramProb s = (\out _ ->
    let a = Eye (rows out) 1.0
        b = Vector (rows out) s
    -- create a new variable named "t0z0" (or in that form)
    in Problem (Just out) [[(out, a)]] [b] [])
    -- minimize out s.t. out == s
  
  -- constructors (need to take size as arguments)
  positiveParameter :: String -> (Int, Int) -> CVXSymbol
  positiveParameter s sz 
    = Parameter s (sizeFunc sz) Affine positiveSign (paramProb s)
  
  negativeParameter :: String -> (Int, Int) -> CVXSymbol
  negativeParameter s sz 
    = Parameter s (sizeFunc sz) Affine negativeSign (paramProb s)
  
  parameter :: String -> (Int,Int) -> CVXSymbol
  parameter s sz 
    = Parameter s (sizeFunc sz) Affine unknownSign (paramProb s)
  
  variable :: String -> (Int, Int) -> CVXSymbol
  variable s sz 
    = Variable s (sizeFunc sz) Affine unknownSign
  
  
  -- check tree validity, easy function to make sure that ParamFunction have 
  -- *parameters* on one side
  isValidExpr :: CVXExpression -> Bool
  isValidExpr (Leaf _) = True
  isValidExpr (Node f args) = 
    let (arguments, parameters) = splitAt (nargs f) args -- assumes parameters follow arguments
        areExprs = map isValidExpr arguments
        areParams = map isParam parameters
    in True && and areExprs && and areParams && areValidArgs f (map size args)
  
  isParam :: CVXExpression -> Bool
  isParam (Leaf (Parameter _ _ _ _ _)) = True
  isParam _ = False
  
