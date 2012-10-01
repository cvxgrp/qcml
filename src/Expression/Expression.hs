module Expression.Expression (positiveSign,
  negativeSign,
  unknownSign,
  positiveParameter,
  negativeParameter,
  positiveVariable,
  negativeVariable,
  parameter,
  variable,
  module Expression.DCP,
  module Expression.SOCP) where
    
  import Expression.DCP
  import Expression.SOCP
  -- quad over lin x y = minimize t subject to ((y+t)/2, (y-t)/2, x) in SOC3, y in SOC1

  
  -- constant sign functions
  positiveSign = (\_ -> Positive)
  negativeSign = (\_ -> Negative)
  unknownSign = (\_ -> Unknown)
  
  -- parameter rewriter 
  -- parameter a becomes
  --
  -- minimize t
  -- subject to 
  --    a*z - t == 0
  --    z == 1
  --
  paramProb s = (\out _ ->
    -- create a new variable named "t0z0" (or in that form)
    Problem (out) [[(out,"1")]] [s] [])
    -- minimize out s.t. out == s
  
  -- constructors
  positiveParameter :: String -> CVXSymbol
  positiveParameter s = Parameter s Affine positiveSign (paramProb s)
  
  negativeParameter :: String -> CVXSymbol
  negativeParameter s = Parameter s Affine negativeSign (paramProb s)
  
  parameter :: String -> CVXSymbol
  parameter s = Parameter s Affine unknownSign (paramProb s)
  
  positiveVariable :: String -> CVXSymbol
  positiveVariable s = Variable s Affine positiveSign
  
  negativeVariable :: String -> CVXSymbol
  negativeVariable s = Variable s Affine negativeSign
  
  variable :: String -> CVXSymbol
  variable s = Variable s Affine unknownSign
  
  
  
  -- check tree validity
  -- TODO, easy function to make sure that ParamFunction have *parameters* on one side
  

  
