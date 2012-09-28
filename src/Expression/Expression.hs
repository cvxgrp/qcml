module Expression.Expression (positiveSign,
  negativeSign,
  unknownSign,
  positiveParameter,
  negativeParameter,
  positiveVariable,
  negativeVariable,
  parameter,
  variable,
  module Expression.DCP) where
    
  import Expression.DCP
  -- quad over lin x y = minimize t subject to ((y+t)/2, (y-t)/2, x) in SOC3, y in SOC1

  
  -- constant sign functions
  positiveSign = (\_ -> Positive)
  negativeSign = (\_ -> Negative)
  unknownSign = (\_ -> Unknown)
  
  -- constructors
  positiveParameter :: String -> CVXSymbol
  positiveParameter s = Parameter s Affine positiveSign
  
  negativeParameter :: String -> CVXSymbol
  negativeParameter s = Parameter s Affine negativeSign
  
  parameter :: String -> CVXSymbol
  parameter s = Parameter s Affine unknownSign
  
  positiveVariable :: String -> CVXSymbol
  positiveVariable s = Variable s Affine positiveSign
  
  negativeVariable :: String -> CVXSymbol
  negativeVariable s = Variable s Affine negativeSign
  
  variable :: String -> CVXSymbol
  variable s = Variable s Affine unknownSign
  
  
  
  -- check tree validity
  -- TODO, easy function to make sure that ParamFunction have *parameters* on one side
  

  
