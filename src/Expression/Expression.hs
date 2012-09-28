module Expression.Expression (Expression(..)) where
  import DCP.DCP
  
  data Expression a = Empty 
    | Leaf a 
    | BinaryNode a (Expression a) (Expression a)
    | UnaryNode a (Expression a)
    deriving (Show)
  
  -- parameter can be promoted to expression
  -- expression cannot be demoted to parameter
  data CVXSymbol
    = Parameter { 
      name::String, 
      vexity::Vexity, 
      sign::SignFunc
    }
    | Variable { 
      name::String, 
      vexity::Vexity, 
      sign::SignFunc 
    }
    | Function { 
      name::String, 
      vexity::Vexity, 
      sign::SignFunc, 
      monotonicity::MonoFunc 
    }
    | ParamFunction {
      name::String, 
      vexity::Vexity, 
      sign::SignFunc, 
      monotonicity::MonoFunc
    }

  instance Show CVXSymbol where
    show x = name x
  
  -- typedefs to reduce "typing" of code
  type CVXExpression = Expression CVXSymbol
  type SignFunc = [Sign]->Sign
  type MonoFunc = [Sign]->[Monotonicity]
  
  -- test expressions
  expr1 = BinaryNode plusFunc (Leaf $ parameter "x") (Leaf $ positiveParameter "y")
  expr2 = BinaryNode (quadOverLinFunc) (UnaryNode (squareFunc) expr1) (Leaf (variable "y"))
  expr3 = BinaryNode plusFunc expr2 (Leaf $ variable "z")
  
  expr4 = UnaryNode squareFunc (Leaf $ variable "a")
  expr5 = UnaryNode squareFunc expr4
  expr6 = BinaryNode plusFunc expr4 (Leaf $ positiveParameter "1")
  expr7 = BinaryNode plusFunc expr4 (Leaf $ negativeParameter "-1")
  expr8 = UnaryNode squareFunc expr6 --(x^2+1)^2
  expr9 = UnaryNode squareFunc expr7 --(x^2-1)^2
  
  expr10 = BinaryNode mulFunc (Leaf $ positiveParameter "3") (Leaf $ variable "z")
  expr11 = UnaryNode squareFunc expr10
  
  -- constant sign functions
  positiveSignFunc = (\_ -> Positive)
  negativeSignFunc = (\_ -> Negative)
  unknownSignFunc = (\_ -> Unknown)
  
  -- constructors
  positiveParameter :: String -> CVXSymbol
  positiveParameter s = Parameter s Affine positiveSignFunc
  
  negativeParameter :: String -> CVXSymbol
  negativeParameter s = Parameter s Affine negativeSignFunc
  
  parameter :: String -> CVXSymbol
  parameter s = Parameter s Affine unknownSignFunc
  
  positiveVariable :: String -> CVXSymbol
  positiveVariable s = Variable s Affine positiveSignFunc
  
  negativeVariable :: String -> CVXSymbol
  negativeVariable s = Variable s Affine negativeSignFunc
  
  variable :: String -> CVXSymbol
  variable s = Variable s Affine unknownSignFunc
  
  -- this might be one way to provide monotonicity
  squareMonotonicity :: [Sign]->[Monotonicity]
  squareMonotonicity [x] = [(quadOverLinMonotonicity [x,Positive])!!0]
  
  quadOverLinMonotonicity :: [Sign] -> [Monotonicity]
  quadOverLinMonotonicity [Positive, _] = [Increasing, Decreasing]
  quadOverLinMonotonicity [Negative, _] = [Decreasing, Decreasing]
  quadOverLinMonotonicity _ = [Nonmonotone, Decreasing]
  
  -- this is how you define atoms now (belongs in "Atoms.hs")
  squareFunc :: CVXSymbol
  squareFunc = Function {
    name="square",
    vexity=Convex,
    sign=positiveSignFunc,
    monotonicity=(\x -> case(x) of
      [Positive] -> [Increasing]
      [Negative] -> [Decreasing]
      otherwise -> [Nonmonotone]
    )}
  
  quadOverLinFunc :: CVXSymbol
  quadOverLinFunc = Function {
    name="quad_over_lin",
    vexity=Convex,
    sign=positiveSignFunc,
    monotonicity=(\x -> case (x) of
      [Positive, _] -> [Increasing, Decreasing]
      [Negative, _] -> [Decreasing, Decreasing]
      otherwise -> [Nonmonotone,Decreasing]
      )}
  
  plusFunc :: CVXSymbol
  plusFunc = Function {
    name="plus",
    vexity=Affine,
    sign=(\x -> case(x) of
      [Positive,Positive] -> Positive
      [Negative,Negative] -> Negative
      otherwise -> Unknown
    ),
    monotonicity=(\x -> [Increasing, Increasing])
    }
  
  -- this is how you define *parameterized* atoms
  -- hmm, no different... only restriction happens when we *construct* the tree...
  -- alternative is to have ParamFunction take a Parameter as a type constructor argument
  -- not sure if that will make a difference during code generation
  mulFunc :: CVXSymbol
  mulFunc = ParamFunction {
    name="multiply",
    vexity=Affine,
    sign=(\x -> case (x) of
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
  
  -- check tree validity
  -- TODO, easy function to make sure that ParamFunction have *parameters* on one side
  
  -- DCP rules
  
  isConvex :: CVXExpression -> Bool
  isConvex x = not (expressionVexity x == Concave)
  
  -- only need to export expressionVexity and expressionSign
  expressionVexity :: CVXExpression -> Vexity
  expressionVexity (Leaf x) = vexity x
  expressionVexity (BinaryNode x left right) = 
    infer x [left,right]
  expressionVexity (UnaryNode x rest) =
    infer x [rest]
  expressionVexity _ = Nonconvex

  expressionSign :: CVXExpression -> Sign
  expressionSign (Leaf x) = sign x []
  expressionSign (BinaryNode x left right) =
    sign x [expressionSign left, expressionSign right]
  expressionSign (UnaryNode x rest) = 
    sign x [expressionSign rest]
  expressionSign _ = Unknown

  -- helper functions for DCP rules
  infer :: CVXSymbol -> [CVXExpression] -> Vexity
  infer (Parameter _ v _) _ = v -- these are errors, really
  infer (Variable _ v _) _ = v -- this is also an error
  infer x y = 
    let m = monotonicity x (map expressionSign y)
        v = map expressionVexity y
    in foldl reduceVex (vexity x) (zipWith inferVexity v m)
  
  -- composition rule
  inferVexity :: Vexity -> Monotonicity -> Vexity
  inferVexity Convex Increasing = Convex
  inferVexity Convex Decreasing = Concave
  inferVexity Concave Decreasing = Convex
  inferVexity Concave Increasing = Concave
  inferVexity Affine _ = Affine
  inferVexity _ _ = Nonconvex
  
  -- given a list of vexities, gives the "max"
  -- i.e., Nonconvex > Affine > Convex = Concave
  -- er, just produces the "common" vexity
  -- [Affine, Affine, Affine] -> Affine
  -- [Affine, Convex, Convex] -> Convex
  -- etc.
  -- useful for inferring vexity of function (gives vexity in each arg)
  reduceVex :: Vexity -> Vexity -> Vexity
  reduceVex Nonconvex _ = Nonconvex
  reduceVex _ Nonconvex = Nonconvex
  reduceVex Affine s = s
  reduceVex s Affine = s
  reduceVex Convex Convex = Convex
  reduceVex Concave Concave = Concave
  reduceVex _ _ = Nonconvex
  
