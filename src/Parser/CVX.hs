module Parser.CVX (cvxProg, CVXParser, lexer, symbolTable,
  module Text.ParserCombinators.Parsec,
  module P) where
  import qualified Expression.Expression as E
  import qualified Data.Map as M
  import qualified CodeGenerator.Common as C(Codegen(..))
  import Atoms.Atoms
  
  import Data.Maybe
  
  import qualified Text.ParserCombinators.Parsec.Token as P
  import Text.ParserCombinators.Parsec.Language
  import Text.ParserCombinators.Parsec
  import Text.ParserCombinators.Parsec.Expr
  
  builtinFunctions =
    [("square", atom_square),
     ("quad_over_lin", atom_quad_over_lin),
     ("inv_pos", atom_inv_pos),
     ("max", atom_max),
     ("min", atom_min),
     ("pos", atom_pos),
     ("neg", atom_neg),
     ("sum", atom_sum),
     ("abs", atom_abs),
     ("norm2", atom_norm),
     ("norm_inf", atom_norm_inf),
     ("norm1", atom_norm1),
     ("norm", atom_norm),
     ("sqrt", atom_sqrt),
     ("geo_mean", atom_geo_mean),
     ("pow_rat", atom_pow_rat)]

  symbolTable = CVXState M.empty M.empty 0

  data CVXState = CVXState {
      symbols :: M.Map String E.Expr,
      dimensions :: M.Map String Int,
      varcount :: Int
    }

  incrCount :: CVXState -> CVXState
  incrCount state
    = CVXState (symbols state) (dimensions state) (1 + varcount state)

  insertSymbol :: E.Expr -> CVXState -> CVXState
  insertSymbol x state 
    = CVXState newSymbols (dimensions state) (varcount state)
      where newSymbols = M.insert (E.name x) x (symbols state)

  insertDim :: (String, Int) -> CVXState -> CVXState
  insertDim (s,i) state 
    = CVXState (symbols state) newDimensions (varcount state)
      where newDimensions = M.insert s i (dimensions state)
    
  -- type CVXState = M.Map String E.CVXSymbol
  type CVXParser a = GenParser Char CVXState a

  lexer :: P.TokenParser CVXState
  lexer = P.makeTokenParser (emptyDef {
      commentLine = "#",
      identStart = letter <|> char '_',
      identLetter = alphaNum <|> char '_',
      reservedNames = ["minimize", "maximize", "subject to", "parameter", "variable", "dimension", "nonnegative", "nonpositive", "positive", "negative"]
         ++ (map fst builtinFunctions),
      reservedOpNames = ["*", "+", "-", "'", "=", "==", "<=", ">="]
    })
  
  identifier = P.identifier lexer
  whiteSpace = P.whiteSpace lexer
  reserved = P.reserved lexer
  reservedOp = P.reservedOp lexer 
  parens = P.parens lexer
  brackets = P.brackets lexer
  comma = P.comma lexer
  semi = P.semi lexer
  natural = P.natural lexer
  naturalOrFloat = P.naturalOrFloat lexer

  expr :: CVXParser E.Expr
  expr = do {
    e <- buildExpressionParser table term;
    case e of
      E.None s -> fail s
      otherwise -> return e
    } <?> "expression"
  
  unaryNegate :: Int -> E.Expr -> E.Expr
  unaryNegate t a = ecos_negate a (show t)
  
  -- a significant difference from the paper is that parameters are also
  -- expressions, so we can multiply two things of the same *type*
  --
  -- TODO/XXX: parsec handles the precedence for me, but i can't force multiply
  -- to be a *unary* function parameterized by the first "term"
  multiply :: Int -> E.Expr -> E.Expr -> E.Expr
  multiply t a b = ecos_mult a b (show t)
                  
  add :: Int -> E.Expr -> E.Expr -> E.Expr
  add t a b = ecos_plus a b (show t)
          
  minus :: Int -> E.Expr -> E.Expr -> E.Expr
  minus t a b = ecos_minus a b (show t)
  
  -- constructors to help build the expression table
  binary name fun assoc 
    = Infix (do{ 
      reservedOp name; 
      t <- getState; 
      updateState incrCount; 
      return $ fun (varcount t)}) assoc
  prefix name fun
    = Prefix (do{ 
      reservedOp name; 
      t <- getState; 
      updateState incrCount; 
      return $ fun (varcount t) })
  -- parsec doesn't play nice with "mu'*x", since it can't parse
  -- a transpose followed immediately by a multiply
  -- instead, i have it gobble a *single* character (since i know transpose
  -- is a single character operator), and then eat any whitespace following
  postfix name fun
    = Postfix (do { char name; whiteSpace; return fun })  
  
  -- XXX: precedence ordering is *mathematical* precedence (not C-style)
  table = [ [postfix '\'' ecos_transpose],
            [binary "*" multiply AssocRight],
            [prefix "-" unaryNegate],
            [binary "+" add AssocLeft, 
             binary "-" minus AssocLeft]] 
  
  -- a term is made up of "(expr)", functions thereof, parameters, or 
  -- variables
  term = parens expr
      <|> choice (map snd builtinFunctions)
      <|> parameter
      <|> variable
      <|> constant
      <|> concatenation
      <?> "simple expressions"
  
  vertConcatArgs :: CVXParser [E.Expr]
  vertConcatArgs = do {
    sepBy expr semi
  }
  
  concatenation :: CVXParser E.Expr
  concatenation = do { 
    args <- brackets vertConcatArgs;
    t <- getState; 
    updateState incrCount; 
    case (args) of
      [] -> fail "Attempted to concatenate empty expressions"
      [x] -> return x
      x -> return (ecos_concat x (show $ varcount t))
  }
  
  variable :: CVXParser E.Expr
  variable = do { 
    s <- identifier;
    t <- getState; 
    case (M.lookup s (symbols t)) of
      Just x -> return x
      _ -> fail $ "expected a variable but got " ++ s 
  } <?> "variable"
  
  parameter :: CVXParser E.Expr
  parameter = do { 
    s <- identifier;
    t <- getState;
    case (M.lookup s (symbols t)) of
      Just x -> return x
      _ -> fail $ "expected a parameter but got " ++ s 
  } <?> "parameter"
  
  constant :: CVXParser E.Expr
  constant = do {
    s <- naturalOrFloat;
    return $ E.Constant (either fromIntegral id s)
  } <?> "constant"
             
  boolOp :: CVXParser String
  boolOp = do {
    reserved "==";
    return "=="
  } <|> do {
    reserved "<=";
    return "<="
  } <|> do {
    reserved ">=";
    return ">="
  } <?> "boolean operator"
    
  constraint :: CVXParser E.ConicSet
  constraint = do {
    lhs <- expr;
    p <- boolOp;
    rhs <- expr;
    
    t <- getState; 
    updateState incrCount; 
    let result = case (p) of
          "==" -> (ecos_eq lhs rhs)
          "<=" -> (ecos_leq lhs rhs (show $ varcount t))
          ">=" -> (ecos_geq lhs rhs (show $ varcount t))
    in case (result) of
      Just x -> return x
      _ -> fail "Not a signed DCP compliant restraint or dimension mismatch."
  } <?> "constraint"
  
  constraints :: CVXParser [E.ConicSet]
  constraints = do { 
    reserved "subject to";
    result <- many constraint;
    return result 
  } <?> "constraints"
  
  objective :: E.Sense -> CVXParser E.Expr
  objective v = do {
    obj <- expr;
    case(E.vexity obj, v) of
      (E.Affine, _) -> return obj
      (_,E.Find) -> return obj -- or 0?
      (E.Convex, E.Minimize) -> return obj
      (E.Concave, E.Maximize) -> return obj
      _ -> fail $ (show (E.vexity obj) ++ " objective does not agree with sense: " ++ show v)
  } <?> "objective"
  
  sense :: CVXParser E.Sense
  sense = 
    do {
      reserved "minimize";
      return E.Minimize
    } <|>
    do {
      reserved "maximize";
      return E.Maximize
    } <|>
    do {
      reserved "find";
      return E.Find
    }
    <?> "problem sense (maximize or minimize or find)"
  
  problem :: CVXParser E.SOCP
  problem = 
    do {
      probSense <- sense;
      obj <- objective probSense;
      cones <- optionMaybe constraints;
      
      if(isScalar obj)
      then
        let prob = E.socp obj
        in case (cones) of
          Just x -> return (E.SOCP probSense (E.obj prob) (foldr (E.<++>) (E.constraints prob) x))
          _ -> return (E.SOCP probSense (E.obj prob) (E.constraints prob))
      else
        fail $ "expected scalar objective; got objective with " ++ show (E.rows obj) ++ " rows and " ++ show (E.cols obj) ++ " columns."
    } <?> "problem"
  
  dimension :: CVXParser Int
  dimension = 
    do {
      s <- identifier;
      t <- getState;
      case (M.lookup s (dimensions t)) of
        Just x -> return x
        _ -> fail $ "expected a dimension but got identifier " ++ s ++ "instead"
    } <|>
    do {
      dim <- natural;
      if(dim == 0)
      then
        fail $ "expecting nonzero dimension"
      else
        return (fromInteger dim);
    } <?> "dimension"

  shape :: CVXParser (Int, Int)
  shape = 
    do {
      dims <- parens (sepBy dimension comma);
      case(length dims) of
        1 -> return (dims!!0, 1)
        2 -> return (dims!!0, dims!!1)
        _ -> fail $ "wrong number of dimensions"
    } <?> "shape"
  
  modifier :: CVXParser E.Sign
  modifier = 
    do {
      reserved "positive";
      return E.Positive
    } <|>
    do {
      reserved "negative";
      return E.Negative
    } <|>
    do {
      reserved "nonnegative";
      return E.Positive
    } <|>
    do {
      reserved "nonpositive";
      return E.Negative
    } <?> "sign modifier"

  defVariable :: CVXParser ()
  defVariable = do {
    reserved "variable";
    s <- identifier; 
    size <- optionMaybe shape;
    let (m,n) = (fromMaybe (1,1) size)
    in if (n == 1) then
      updateState (insertSymbol (E.variable s (m,n)))
    else
      fail $ "only vector variables are allowed. you attempted to create a matrix variable."
  } <?> "variable"
  
  defParameter :: CVXParser ()
  defParameter = do {
    reserved "parameter";
    s <- identifier;
    sign <- optionMaybe modifier;
    size <- optionMaybe shape;
    let dim = fromMaybe (1,1) size
        p = case (sign) of
          Just E.Positive -> E.parameter s E.Positive dim
          Just E.Negative -> E.parameter s E.Negative dim
          _ -> E.parameter s E.Unknown dim
    in updateState (insertSymbol p)
  } <?> "parameter"

  defDimension :: CVXParser ()
  defDimension = do {
    reserved "dimension";
    s <- identifier;
    reserved "=";
    dim <- natural;
    if(dim == 0)
    then
      fail $ "expecting nonzero dimension";
    else
      updateState (insertDim (s,fromInteger dim));
  } <?> "dimension"
  

  definitions :: CVXParser ()
  definitions =  defVariable <|> defParameter <|> defDimension
    <?> "definitions"

  
  cvxProg :: CVXParser C.Codegen
  cvxProg = do {
    whiteSpace;
    many definitions;
    p <- problem;
    t <- getState;
    eof;
    return $ C.Codegen p (symbols t)
  } <?> "problem"


{-- atom parsers --}
  args :: String -> Int -> CVXParser [E.Expr]
  args s n = do {
    arguments <- sepBy expr comma;
    if (length arguments /= n)
    then fail $ s ++ ": number of arguments do not agree"
    else return arguments
  }

  atom_square :: CVXParser E.Expr
  atom_square = do {
    reserved "square";
    args <- parens $ args "square" 1;
    t <- getState; 
    updateState incrCount; 
    return $ ecos_square (args!!0) (show $ varcount t)
  }

  atom_quad_over_lin :: CVXParser E.Expr
  atom_quad_over_lin = do {
    reserved "quad_over_lin";
    args <- parens $ args "quad_over_lin" 2;
    t <- getState; 
    updateState incrCount; 
    return $ ecos_quad_over_lin (args!!0) (args!!1) (show $ varcount t)
  }

  atom_inv_pos :: CVXParser E.Expr 
  atom_inv_pos = do {
    reserved "inv_pos";
    args <- parens $ args "inv_pos" 1;
    t <- getState; 
    updateState incrCount; 
    return $ ecos_inv_pos (args!!0) (show $ varcount t)
  }

  atom_max :: CVXParser E.Expr 
  atom_max = do {
    reserved "max";
    args <- parens $ args "max" 1;
    t <- getState; 
    updateState incrCount; 
    return $ ecos_max (args!!0) (show $ varcount t)
  }

  atom_min :: CVXParser E.Expr 
  atom_min = do {
    reserved "min";
    args <- parens $ args "min" 1;
    t <- getState; 
    updateState incrCount; 
    return $ ecos_min (args!!0) (show $ varcount t)
  }
  
  atom_pos :: CVXParser E.Expr 
  atom_pos = do {
    reserved "pos";
    args <- parens $ args "pos" 1;
    t <- getState; 
    updateState incrCount; 
    return $ ecos_pos (args!!0) (show $ varcount t)
  }

  atom_neg :: CVXParser E.Expr 
  atom_neg = do {
    reserved "neg";
    args <- parens $ args "neg" 1;
    t <- getState; 
    updateState incrCount; 
    return $ ecos_neg (args!!0) (show $ varcount t)
  }

  atom_sum :: CVXParser E.Expr 
  atom_sum = do {
    reserved "sum";
    args <- parens $ args "sum" 1;
    t <- getState; 
    updateState incrCount; 
    return $ ecos_sum (args!!0) (show $ varcount t)
  }

  atom_norm :: CVXParser E.Expr 
  atom_norm = do {
    reserved "norm";
    args <- parens $ args "norm" 1;
    t <- getState; 
    updateState incrCount; 
    return $ ecos_norm (args!!0) (show $ varcount t)
  } <|> do {
    reserved "norm2";
    args <- parens $ args "norm2" 1;
    t <- getState; 
    updateState incrCount; 
    return $ ecos_norm (args!!0) (show $ varcount t)
  }

  atom_abs :: CVXParser E.Expr 
  atom_abs = do {
    reserved "abs";
    args <- parens $ args "abs" 1;
    t <- getState; 
    updateState incrCount; 
    return $ ecos_abs (args!!0) (show $ varcount t)
  } 

  atom_norm_inf :: CVXParser E.Expr 
  atom_norm_inf = do {
    reserved "norm_inf";
    args <- parens $ args "norm_inf" 1;
    t <- getState; 
    updateState incrCount; 
    return $ ecos_norm_inf (args!!0) (show $ varcount t)
  }

  atom_norm1 :: CVXParser E.Expr 
  atom_norm1 = do {
    reserved "norm1";
    args <- parens $ args "norm1" 1;
    t <- getState; 
    updateState incrCount; 
    return $ ecos_norm1 (args!!0) (show $ varcount t)
  }

  atom_sqrt :: CVXParser E.Expr 
  atom_sqrt = do {
    reserved "sqrt";
    args <- parens $ args "sqrt" 1;
    t <- getState; 
    updateState incrCount; 
    return $ ecos_sqrt (args!!0) (show $ varcount t)
  }

  atom_geo_mean :: CVXParser E.Expr
  atom_geo_mean = do {
    reserved "geo_mean";
    args <- parens $ args "geo_mean" 2;
    t <- getState; 
    updateState incrCount; 
    return $ ecos_geo_mean (args!!0) (args!!1) (show $ varcount t)
  }

  atom_pow_rat :: CVXParser E.Expr
  atom_pow_rat = do {
    reserved "pow_rat";
    (arg, pqs) <- parens pow_rat_args;
    t <- getState; 
    updateState incrCount; 
    return $ ecos_pow_rat arg (pqs!!0) (pqs!!1) (show $ varcount t)
  }

  -- XXX/TODO: could make arguments optional if needed
  pow_rat_args :: CVXParser (E.Expr, [Integer])
  pow_rat_args = do {
    e <- expr;
    comma;
    p <- natural;
    comma;
    q <- natural;
    return (e, [p,q])
  } <?> "pow_rat arguments"


