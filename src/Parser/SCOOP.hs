{--

Copyright (c) 2012-2013, Eric Chu (eytchu@gmail.com)
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met: 

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer. 
2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution. 

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.

The views and conclusions contained in the software and documentation are
those of the authors and should not be interpreted as representing official
policies, either expressed or implied, of the FreeBSD Project.

--}

module Parser.SCOOP (cvxProg, ScoopParser, lexer, symbolTable,
  module Text.ParserCombinators.Parsec,
  module P) where
  import qualified Expression.Expression as E
  import qualified Data.Map as M
  import qualified Data.Set as S
  --import qualified CodeGenerator.Common as C(Codegen(..))
  --import Atoms.Atoms
  
  import Data.Maybe
  
  import qualified Text.ParserCombinators.Parsec.Token as P
  import Text.ParserCombinators.Parsec.Language
  import Text.ParserCombinators.Parsec
  import Text.ParserCombinators.Parsec.Expr
  
  --builtinFunctions =
  --  [("square", atom_square),
  --   ("quad_over_lin", atom_quad_over_lin),
  --   ("inv_pos", atom_inv_pos),
  --   ("max", atom_max),
  --   ("min", atom_min),
  --   ("pos", atom_pos),
  --   ("neg", atom_neg),
  --   ("sum", atom_sum),
  --   ("abs", atom_abs),
  --   ("norm2", atom_norm),
  --   ("norm_inf", atom_norm_inf),
  --   ("norm1", atom_norm1),
  --   ("norm", atom_norm),
  --   ("sqrt", atom_sqrt),
  --   ("geo_mean", atom_geo_mean),
  --   ("pow_rat", atom_pow_rat),
  --   ("diag", atom_diag)]

  symbolTable = ScoopState M.empty S.empty 0

  data ScoopState = ScoopState {
      symbols :: M.Map String E.Expr,
      dimensions :: S.Set String,
      varcount :: Integer
    }

  incrCount :: ScoopState -> ScoopState
  incrCount state
    = ScoopState (symbols state) (dimensions state) (1 + varcount state)

  insertSymbol :: E.Expr -> ScoopState -> ScoopState
  insertSymbol x state 
    = ScoopState newSymbols (dimensions state) (varcount state)
      where newSymbols = M.insert (E.name x) x (symbols state)

  insertDim :: String -> ScoopState -> ScoopState
  insertDim s state 
    = ScoopState (symbols state) newDimensions (varcount state)
      where newDimensions = S.insert s (dimensions state)
    
  type ScoopParser a = GenParser Char ScoopState a

  lexer :: P.TokenParser ScoopState
  lexer = P.makeTokenParser (emptyDef {
      commentLine = "#",
      identStart = letter <|> char '_',
      identLetter = alphaNum <|> char '_',
      reservedNames = ["minimize", "maximize", "subject to", "parameter", "variable", "dimension", "nonnegative", "nonpositive", "positive", "negative"],
        -- ++ (map fst builtinFunctions),
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

  --expr :: ScoopParser E.Expr
  --expr = do {
  --  e <- buildExpressionParser table term;
  --  case e of
  --    E.None s -> fail s
  --    otherwise -> return e
  --  } <?> "expression"
  
  --unaryNegate :: Integer -> E.Expr -> E.Expr
  --unaryNegate t a = scoop_negate a (show t)
  
  ---- a significant difference from the paper is that parameters are also
  ---- expressions, so we can multiply two things of the same *type*
  ----
  ---- TODO/XXX: parsec handles the precedence for me, but i can't force multiply
  ---- to be a *unary* function parameterized by the first "term"
  --multiply :: Integer -> E.Expr -> E.Expr -> E.Expr
  --multiply t a b = scoop_mult a b (show t)
                  
  --add :: Integer -> E.Expr -> E.Expr -> E.Expr
  --add t a b = scoop_plus a b (show t)
          
  --minus :: Integer -> E.Expr -> E.Expr -> E.Expr
  --minus t a b = scoop_minus a b (show t)
  
  ---- constructors to help build the expression table
  --binary name fun assoc 
  --  = Infix (do{ 
  --    reservedOp name; 
  --    t <- getState; 
  --    updateState incrCount; 
  --    return $ fun (varcount t)}) assoc
  --prefix name fun
  --  = Prefix (do{ 
  --    reservedOp name; 
  --    t <- getState; 
  --    updateState incrCount; 
  --    return $ fun (varcount t) })
  ---- parsec doesn't play nice with "mu'*x", since it can't parse
  ---- a transpose followed immediately by a multiply
  ---- instead, i have it gobble a *single* character (since i know transpose
  ---- is a single character operator), and then eat any whitespace following
  --postfix name fun
  --  = Postfix (do { char name; whiteSpace; return fun })  
  
  ---- XXX: precedence ordering is *mathematical* precedence (not C-style)
  --table = [ [postfix '\'' scoop_transpose],
  --          [binary "*" multiply AssocRight],
  --          [prefix "-" unaryNegate],
  --          [binary "+" add AssocLeft, 
  --           binary "-" minus AssocLeft]] 
  
  ---- a term is made up of "(expr)", functions thereof, parameters, or 
  ---- variables
  --term = parens expr
  --    <|> choice (map snd builtinFunctions)
  --    <|> parameter
  --    <|> variable
  --    <|> constant
  --    <|> concatenation
  --    <?> "simple expressions"
  
  --vertConcatArgs :: ScoopParser [E.Expr]
  --vertConcatArgs = do {
  --  sepBy expr semi
  --}
  
  --concatenation :: ScoopParser E.Expr
  --concatenation = do { 
  --  args <- brackets vertConcatArgs;
  --  t <- getState; 
  --  updateState incrCount; 
  --  case (args) of
  --    [] -> fail "Attempted to concatenate empty expressions"
  --    [x] -> return x
  --    x -> return (scoop_concat x (show $ varcount t))
  --}
  
  --variable :: ScoopParser E.Expr
  --variable = do { 
  --  s <- identifier;
  --  t <- getState; 
  --  case (M.lookup s (symbols t)) of
  --    Just x -> return x
  --    _ -> fail $ "expected a variable but got " ++ s 
  --} <?> "variable"
  
  --parameter :: ScoopParser E.Expr
  --parameter = do { 
  --  s <- identifier;
  --  t <- getState;
  --  case (M.lookup s (symbols t)) of
  --    Just x -> return x
  --    _ -> fail $ "expected a parameter but got " ++ s 
  --} <?> "parameter"
  
  --constant :: ScoopParser E.Expr
  --constant = do {
  --  s <- naturalOrFloat;
  --  return $ E.Constant (either fromIntegral id s)
  --} <?> "constant"
             
  --boolOp :: ScoopParser String
  --boolOp = do {
  --  reserved "==";
  --  return "=="
  --} <|> do {
  --  reserved "<=";
  --  return "<="
  --} <|> do {
  --  reserved ">=";
  --  return ">="
  --} <?> "boolean operator"
    
  --constraint :: ScoopParser E.ConicSet
  --constraint = do {
  --  lhs <- expr;
  --  p <- boolOp;
  --  rhs <- expr;
    
  --  t <- getState; 
  --  updateState incrCount; 
  --  let result = case (p) of
  --        "==" -> (scoop_eq lhs rhs)
  --        "<=" -> (scoop_leq lhs rhs (show $ varcount t))
  --        ">=" -> (scoop_geq lhs rhs (show $ varcount t))
  --  in case (result) of
  --    Just x -> return x
  --    _ -> fail "Not a signed DCP compliant restraint or dimension mismatch."
  --} <?> "constraint"
  
  --constraints :: ScoopParser [E.ConicSet]
  --constraints = do { 
  --  reserved "subject to";
  --  result <- many constraint;
  --  return result 
  --} <?> "constraints"
  
  objFunc :: E.Curvature -> ScoopParser String --E.Expr
  objFunc v = do {
    obj <- identifier; -- expr;
    return obj
    --case(E.vexity obj, v) of
    --  (E.Affine, _) -> return obj
    --  (_,E.Affine) -> return obj -- or 0?
    --  (E.Convex, E.Convex) -> return obj
    --  (E.Concave, E.Concave) -> return obj
    --  _ -> fail $ (show (E.vexity obj) ++ " objective does not agree with sense: " ++ show v)
  } <?> "objective function"
  
  sense :: ScoopParser E.Curvature
  sense = 
    do {
      reserved "minimize";
      return E.Convex
    } <|>
    do {
      reserved "maximize";
      return E.Concave
    } <|>
    do {
      reserved "find";
      return E.Affine
    }
    <?> "problem sense (maximize or minimize or find)"
  
  objective :: ScoopParser ()
  objective = 
    do {
      probSense <- sense;
      obj <- objFunc probSense;
      optionMaybe (reserved "subject to");

      return ()
      -- cones <- optionMaybe constraints;
      
      -- (we don't check that main objective is scalar)
      -- TODO: emit warning instead of failing when objective is an abstract vector (since we don't know sizes--it could be "1"!)
      --if(isScalar obj)
      --then
      --  return () -- probably should return a string
      --else
      --  fail $ "expected scalar objective; got objective with " ++ show 12313 ++ " rows and " ++ show 54321 ++ " columns."
    } <?> "an objective"
  
  dimension :: ScoopParser String
  dimension = 
    do {
      s <- identifier;
      t <- getState;
      if(S.member s (dimensions t))
      then
        return s
      else
        fail $ "nonexistent dimension " ++ s
    } <|>
    do {
      dim <- natural;
      if(dim == 0)
      then
        fail $ "expecting nonzero dimension"
      else
        return (show dim);
    } <?> "dimension"

  shape :: ScoopParser (String, String)
  shape = 
    do {
      dims <- parens (sepBy dimension comma);
      case(length dims) of
        1 -> return (dims!!0, "1")
        2 -> return (dims!!0, dims!!1)
        _ -> fail $ "wrong number of dimensions"
    } <?> "shape"
  
  modifier :: ScoopParser E.Sign
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

  defVariable :: ScoopParser ()
  defVariable = do {
    reserved "variable";
    s <- identifier; 
    size <- optionMaybe shape;
    let (m,n) = (fromMaybe ("1","1") size)
    in if (n == "1") then
      updateState (insertSymbol (E.Expr s m E.Affine E.Unknown))
    else
      fail $ "only vector variables are allowed. you attempted to create a matrix variable."
  } <?> "variable"
  
  defParameter :: ScoopParser ()
  defParameter = do {
    reserved "parameter";
    s <- identifier;
    sign <- optionMaybe modifier;
    size <- optionMaybe shape;
    let (m,n) = fromMaybe ("1","1") size
        p = case (sign) of
          Just E.Positive -> E.Param s m n E.Positive
          Just E.Negative -> E.Param s m n E.Negative
          _ -> E.Param s m n E.Unknown
    in updateState (insertSymbol p)
  } <?> "parameter"

  defDimension :: ScoopParser ()
  defDimension = do {
    reserved "dimension";
    s <- identifier;

    updateState (insertDim s);
  } <?> "dimension"
  

  line :: ScoopParser ()
  line =  defVariable <|> defParameter <|> defDimension 
    <?> "a line (variable, parameter, dimension definition, or a constraint)"


  -- one file is *one* problem
  -- "minimize", "maximize", or "find" must appear exactly *once*
  cvxProg :: ScoopParser String
  cvxProg = do {
    whiteSpace;
    manyTill line objective;
    whiteSpace;
    many line;
    --p <- problem;
    --t <- getState;
    eof;
    return "Hello"
  } <?> "problem"


{-- atom parsers --}
  --args :: String -> Int -> ScoopParser [E.Expr]
  --args s n = do {
  --  arguments <- sepBy expr comma;
  --  if (length arguments /= n)
  --  then fail $ s ++ ": number of arguments do not agree"
  --  else return arguments
  --}

  --atom_square :: ScoopParser E.Expr
  --atom_square = do {
  --  reserved "square";
  --  args <- parens $ args "square" 1;
  --  t <- getState; 
  --  updateState incrCount; 
  --  return $ scoop_square (args!!0) (show $ varcount t)
  --}

  --atom_quad_over_lin :: ScoopParser E.Expr
  --atom_quad_over_lin = do {
  --  reserved "quad_over_lin";
  --  args <- parens $ args "quad_over_lin" 2;
  --  t <- getState; 
  --  updateState incrCount; 
  --  return $ scoop_quad_over_lin (args!!0) (args!!1) (show $ varcount t)
  --}

  --atom_inv_pos :: ScoopParser E.Expr 
  --atom_inv_pos = do {
  --  reserved "inv_pos";
  --  args <- parens $ args "inv_pos" 1;
  --  t <- getState; 
  --  updateState incrCount; 
  --  return $ scoop_inv_pos (args!!0) (show $ varcount t)
  --}

  --atom_max :: ScoopParser E.Expr 
  --atom_max = do {
  --  reserved "max";
  --  args <- parens $ args "max" 1;
  --  t <- getState; 
  --  updateState incrCount; 
  --  return $ scoop_max (args!!0) (show $ varcount t)
  --}

  --atom_min :: ScoopParser E.Expr 
  --atom_min = do {
  --  reserved "min";
  --  args <- parens $ args "min" 1;
  --  t <- getState; 
  --  updateState incrCount; 
  --  return $ scoop_min (args!!0) (show $ varcount t)
  --}
  
  --atom_pos :: ScoopParser E.Expr 
  --atom_pos = do {
  --  reserved "pos";
  --  args <- parens $ args "pos" 1;
  --  t <- getState; 
  --  updateState incrCount; 
  --  return $ scoop_pos (args!!0) (show $ varcount t)
  --}

  --atom_neg :: ScoopParser E.Expr 
  --atom_neg = do {
  --  reserved "neg";
  --  args <- parens $ args "neg" 1;
  --  t <- getState; 
  --  updateState incrCount; 
  --  return $ scoop_neg (args!!0) (show $ varcount t)
  --}

  --atom_sum :: ScoopParser E.Expr 
  --atom_sum = do {
  --  reserved "sum";
  --  args <- parens $ args "sum" 1;
  --  t <- getState; 
  --  updateState incrCount; 
  --  return $ scoop_sum (args!!0) (show $ varcount t)
  --}

  --atom_norm :: ScoopParser E.Expr 
  --atom_norm = do {
  --  reserved "norm";
  --  args <- parens $ args "norm" 1;
  --  t <- getState; 
  --  updateState incrCount; 
  --  return $ scoop_norm (args!!0) (show $ varcount t)
  --} <|> do {
  --  reserved "norm2";
  --  args <- parens $ args "norm2" 1;
  --  t <- getState; 
  --  updateState incrCount; 
  --  return $ scoop_norm (args!!0) (show $ varcount t)
  --}

  --atom_abs :: ScoopParser E.Expr 
  --atom_abs = do {
  --  reserved "abs";
  --  args <- parens $ args "abs" 1;
  --  t <- getState; 
  --  updateState incrCount; 
  --  return $ scoop_abs (args!!0) (show $ varcount t)
  --} 

  --atom_norm_inf :: ScoopParser E.Expr 
  --atom_norm_inf = do {
  --  reserved "norm_inf";
  --  args <- parens $ args "norm_inf" 1;
  --  t <- getState; 
  --  updateState incrCount; 
  --  return $ scoop_norm_inf (args!!0) (show $ varcount t)
  --}

  --atom_norm1 :: ScoopParser E.Expr 
  --atom_norm1 = do {
  --  reserved "norm1";
  --  args <- parens $ args "norm1" 1;
  --  t <- getState; 
  --  updateState incrCount; 
  --  return $ scoop_norm1 (args!!0) (show $ varcount t)
  --}

  --atom_sqrt :: ScoopParser E.Expr 
  --atom_sqrt = do {
  --  reserved "sqrt";
  --  args <- parens $ args "sqrt" 1;
  --  t <- getState; 
  --  updateState incrCount; 
  --  return $ scoop_sqrt (args!!0) (show $ varcount t)
  --}

  --atom_geo_mean :: ScoopParser E.Expr
  --atom_geo_mean = do {
  --  reserved "geo_mean";
  --  args <- parens $ args "geo_mean" 2;
  --  t <- getState; 
  --  updateState incrCount; 
  --  return $ scoop_geo_mean (args!!0) (args!!1) (show $ varcount t)
  --}

  --atom_pow_rat :: ScoopParser E.Expr
  --atom_pow_rat = do {
  --  reserved "pow_rat";
  --  (arg, pqs) <- parens pow_rat_args;
  --  t <- getState; 
  --  updateState incrCount; 
  --  return $ scoop_pow_rat arg (pqs!!0) (pqs!!1) (show $ varcount t)
  --}

  ---- XXX/TODO: could make arguments optional if needed
  --pow_rat_args :: ScoopParser (E.Expr, [Integer])
  --pow_rat_args = do {
  --  e <- expr;
  --  comma;
  --  p <- natural;
  --  comma;
  --  q <- natural;
  --  return (e, [p,q])
  --} <?> "pow_rat arguments"

  --atom_diag :: ScoopParser E.Expr
  --atom_diag = do {
  --  reserved "diag";
  --  args <- parens $ args "diag" 1;
  --  return $ scoop_diag (args!!0)
  --}

