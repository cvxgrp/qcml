module Parser.CVX (cvxProb, CVXParser, lexer, symbolTable,
  module Text.ParserCombinators.Parsec,
  module Text.ParserCombinators.Parsec.Token) where
  import qualified Expression.Expression as E
  import qualified Data.Map as M
  import Rewriter.Atoms
  
  import Data.Maybe
  
  import Text.ParserCombinators.Parsec.Token
  import Text.ParserCombinators.Parsec.Language
  import Text.ParserCombinators.Parsec
  import Text.ParserCombinators.Parsec.Expr
  
  -- to be removed later
  import Rewriter.ECOS
  
  data CVXState = CVXState {
      symbols :: M.Map String E.CVXSymbol,
      dimensions :: M.Map String Int
    }
    
  symbolInsert :: E.CVXSymbol -> CVXState -> CVXState
  symbolInsert x state 
    = CVXState (M.insert (E.name x) x (symbols state)) (dimensions state)
  
  dimensionInsert :: (String, Int) -> CVXState -> CVXState
  dimensionInsert (s,i) state 
    = CVXState (symbols state) (M.insert s i (dimensions state))
    
  -- type CVXState = M.Map String E.CVXSymbol
  type CVXParser a = GenParser Char CVXState a
  symbolTable = CVXState M.empty M.empty

  lexer :: TokenParser CVXState
  lexer = makeTokenParser (haskellDef {
      reservedNames = ["minimize", "maximize", "subject to", "parameter", "variable", "dimension", "nonnegative", "nonpositive"],
      reservedOpNames = ["*", "+", "-", "=", "==", "<=", ">="] ++ (M.keys ecosAtoms)
    })
    
  expr :: CVXParser E.CVXExpression
  expr = buildExpressionParser table term
          <?> "expression"
  
  -- this function exists since I need to verify that the expression satisfies
  -- the restricted multiply because of arithmetic precedence rules
  cvxExpr :: CVXParser E.CVXExpression
  cvxExpr = do {
    e <- expr;
    if(E.isValidExpr e)
    then
      return e
    else
      fail ("Expression " ++ show e ++ " is not a valid expression. (Dimension mismatch, tried to multiply two expressions, etc.)")
  }
  
  -- unpack list arguments in to actual arguments
  packBinary :: ([E.CVXExpression]->E.CVXExpression) -> E.CVXExpression -> E.CVXExpression -> E.CVXExpression
  packBinary f a b = f [a,b]
  
  packUnary :: ([E.CVXExpression]->E.CVXExpression) -> E.CVXExpression -> E.CVXExpression
  packUnary f a = f [a]
  
  packMul :: ([E.CVXExpression]->E.CVXExpression) -> E.CVXExpression -> E.CVXExpression -> E.CVXExpression
  packMul f a b = f [b,a]
  
  -- constructors to help build the expression table
  binary name fun assoc 
    = Infix (do{ reservedOp lexer name; return fun }) assoc
  prefix name fun
    = Prefix (do{ reservedOp lexer name; return fun })
  
  -- XXX: precedence ordering is *mathematical* precedence (not C-style)
  table = [ [binary "*" (packMul $ E.Node ecosMul) AssocRight],
            [prefix "-" (packUnary $ E.Node ecosNegate)],
            [binary "+" (packBinary $ E.Node ecosPlus) AssocLeft, 
             binary "-" (packBinary $ E.Node ecosMinus) AssocLeft] ] 
  
  -- a term is made up of "(cvxExpr)", functions thereof, parameters, or 
  -- variables
  term = (parens lexer cvxExpr)
      <|> choice (map function (M.keys ecosAtoms))
      <|> try parameter
      <|> variable
      <|> constant
      <|> concatenation
      <?> "simple expressions"
  
  vertConcatArgs :: CVXParser [E.CVXExpression]
  vertConcatArgs = do {
    sepBy cvxExpr (semi lexer)
  }
  
  concatenation :: CVXParser E.CVXExpression
  concatenation = do { 
    args <- brackets lexer vertConcatArgs;
    let f = ecosConcat (length args)
    in case (args) of
      [] -> fail "Attempted to concatenate empty expressions"
      [x] -> return x
      x -> return (E.Node f x)
  }
  
  args :: CVXParser [E.CVXExpression]
  args = do {
    sepBy cvxExpr (comma lexer)
  }
        
  function :: String -> CVXParser E.CVXExpression
  function atomName = 
    let symbol = M.lookup atomName ecosAtoms
        n = case (symbol) of 
          Just x -> E.nargs x
          _ -> 0
        p = case (symbol) of
          Just x -> E.nparams x
          _ -> 0
    in do {
      reserved lexer atomName;
      args <- parens lexer args;
      case (symbol) of
        Just x ->      
          if (length args /= n) 
            then fail "number of arguments do not agree"
            else case (n) of 
              1 -> return (E.Node x args)
              2 -> return (E.Node x args)
              _ -> fail "no support for n-ary arguments"
        _ -> fail "no such atom"
    }
  
  variable :: CVXParser E.CVXExpression
  variable = do { 
    s <- identifier lexer;
    t <- getState; 
    case (M.lookup s (symbols t)) of
      Just x -> return (E.Leaf x)
      _ -> fail $ "expected variable but got " ++ s 
  }
  
  parameter :: CVXParser E.CVXExpression
  parameter = do { 
    s <- identifier lexer;
    t <- getState; 
    case (M.lookup s (symbols t)) of
      Just x -> return (E.Leaf x)
      _ -> fail $ "expected parameter but got " ++ s 
  }
  
  constant :: CVXParser E.CVXExpression
  constant = do {
    s <- naturalOrFloat lexer;
    if(either (>=0) (>=0.0) s)
    then
      return (E.Leaf $ (E.positiveParameter (either show show s) (1,1)))
    else
      return (E.Leaf $ (E.negativeParameter (either show show s) (1,1)))
  }
             
  createVariable :: CVXParser E.CVXSymbol
  createVariable = do { 
    s <- identifier lexer; 
    size <- optionMaybe shape;
    let (m,n) = (fromMaybe (1,1) size)
    in if (n == 1) then
      return (E.variable s (m,n))
    else
      fail $ "only vector variables are allowed. you attempted to create a matrix variable."
  } <?> "variable"
  
  createParameter :: CVXParser E.CVXSymbol
  createParameter = do {
    s <- identifier lexer;
    sign <- optionMaybe modifier;
    size <- optionMaybe shape;
    let dim = fromMaybe (1,1) size
    in case (sign) of
      Just E.Positive -> return (E.positiveParameter s dim)
      Just E.Negative -> return (E.negativeParameter s dim)
      _ -> return (E.parameter s dim)
  } <?> "parameter"
  
  dimension :: CVXParser Int
  dimension = 
    do {
      s <- identifier lexer;
      t <- getState;
      case (M.lookup s (dimensions t)) of
        Just x -> return x
        _ -> fail $ "expected dimension but got " ++ s
    } <|>
    do {
      dim <- natural lexer;
      if(dim == 0)
      then
        fail $ "expecting nonzero dimension"
      else
        return (fromInteger dim);
    } <?> "dimension"
  
  shape :: CVXParser (Int, Int)
  shape = 
    do {
      dims <- parens lexer (sepBy dimension (comma lexer));
      case(length dims) of
        1 -> return (dims!!0, 1)
        2 -> return (dims!!0, dims!!1)
        _ -> fail $ "wrong number of dimensions"
    } <?> "shape"
  
  modifier :: CVXParser E.Sign
  modifier = 
    do {
      reserved lexer "positive";
      return E.Positive
    } <|>
    do {
      reserved lexer "negative";
      return E.Negative
    } <?> "modifier"
  
  boolOp :: CVXParser String
  boolOp = do {
    reserved lexer "==";
    return "=="
    } <|> do {
      reserved lexer "<=";
      return "<="
    } <|> do {
      reserved lexer ">=";
      return ">="
    } <?> "boolean operator"
  
  -- may not need
  promote :: E.CVXExpression -> Int -> E.CVXExpression
  promote (E.Leaf p) n = case (p) of
    E.Parameter s _ _ sign _ -> 
      let constructor = case (sign []) of 
            E.Positive -> E.positiveParameter
            E.Negative -> E.negativeParameter
            otherwise -> E.parameter
          in E.Leaf $ constructor s (n,1)
    E.Variable s _ _ _ -> E.Leaf $ E.variable s (n,1)
  promote x n = E.Node (ecosConcat n) (take n $ repeat x)
    
  constraint :: CVXParser E.CVXConstraint
  constraint = do {
    lhs <- cvxExpr;
    p <- boolOp;
    rhs <- cvxExpr;
    
    let (m1,n1) = E.size lhs
        (m2,n2) = E.size rhs
        haveEqualSizes = (m1 == m2 && n1 == n2)
        lhsScalar = (m1 == 1 && n1 == 1)
        rhsScalar = (m2 == 1 && n2 == 1)
        lhsPromoted = case(not haveEqualSizes && lhsScalar) of
          True -> promote lhs m2
          otherwise -> lhs
        rhsPromoted = case(not haveEqualSizes && rhsScalar) of
          True -> promote rhs m1
          otherwise -> rhs
        result = case (p) of
          "==" -> (E.Eq lhs rhs)
          "<=" -> (E.Leq lhs rhs)
          ">=" -> (E.Geq lhs rhs)
    in if (haveEqualSizes || lhsScalar || rhsScalar) then
      case (E.vexity result) of
        E.Convex -> return result
        _ -> fail "Not a signed DCP compliant constraint."
    else
      fail "Dimension mismatch when forming constraints."
  } <?> "constraint"
  
  constraints :: CVXParser [E.CVXConstraint]
  constraints = do { 
    reserved lexer "subject to";
    result <- many constraint;
    return result 
  } <?> "constraints"
  
  objective :: E.CVXSense -> CVXParser E.CVXExpression
  objective v = do {
    obj<-cvxExpr;
    case(E.vexity obj) of
      vex | vex == (E.vexity v) || vex == E.Affine -> return obj
      _ -> fail ("Objective fails to satisfy DCP rule. Not " ++ show v ++ ".")
  } <?> "objective"
  -- 
  -- eol :: CVXParser String
  -- eol = try (string "; ") 
  --   <|> try (string "\r\n") 
  --   <|> try (string "\n\r") 
  --   <|> string ";" 
  --   <|> string "\n"
  --   <|> string "\r"
  --   <?> "end of line"
  
  sense :: CVXParser E.CVXSense
  sense = do {
    reserved lexer "minimize";
    return E.Minimize
    } <|>
    do {
      reserved lexer "maximize";
      return E.Maximize
    } <?> "problem sense (maximize or minimize)"
  
  cvxLine :: CVXParser (Maybe E.CVXProblem)
  cvxLine = 
    do {
      probSense<-sense;
      obj<-objective probSense;
      -- verify that the expression is a valid parse tree
      c <- optionMaybe constraints;
      t <- getState;
      
      let (m,n) = E.size obj
      in if(m == 1 && m == 1)
      then
        case (c) of
          Just x -> return (Just (E.CVXProblem probSense obj x))
          _ -> return (Just (E.CVXProblem probSense obj []))
      else
        fail $ "expected scalar objective; got objective with " ++ show m ++ " rows and " ++ show n ++ " columns."
    } <|>
    do {
      reserved lexer "parameter";
      p <- createParameter;
      updateState (symbolInsert p);
      -- t <- getState;
      --eol;
      return Nothing
    } <|>
    do {
      reserved lexer "variable";
      v <- createVariable;
      updateState (symbolInsert v);
      -- t <- getState;
      --eol;
      return Nothing
    } <|> 
    do {
      reserved lexer "dimension";
      s <- identifier lexer;
      reserved lexer "=";
      dim <- natural lexer;
      if(dim == 0)
      then
        fail $ "expecting nonzero dimension";
      else
        updateState (dimensionInsert (s,fromInteger dim));
        return Nothing
    } <?> "line"
    -- <|>
    -- do {
    --   obj<-cvxExpr;
    --   --eol;
    --   return (show $ rewrite (E.CVXProblem obj []))
    -- }
  
  cvxEmptyProb = E.CVXProblem E.Minimize (E.Leaf $ E.parameter "0" (1,1)) []
  
  cvxProb :: CVXParser E.CVXProblem
  cvxProb = do {
    whiteSpace lexer;
    result <- many cvxLine;
    eof;
    -- only returns the *first* problem
    return (((fromMaybe cvxEmptyProb).head) $ dropWhile isNothing result)
  } <?> "problem"


