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

  import Atoms.Atoms
  
  import Data.Maybe
  
  import qualified Text.ParserCombinators.Parsec.Token as P
  import Text.ParserCombinators.Parsec.Language
  import Text.ParserCombinators.Parsec
  import Text.ParserCombinators.Parsec.Expr
  
  import qualified Control.Monad.State as St

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

  symbolTable = ScoopState M.empty S.empty

  data ScoopState = ScoopState {
      symbols :: M.Map String E.Symbol, -- vars or parameters
      dimensions :: S.Set String
  }

  insertSymbol :: (E.Symbolic a) => a -> ScoopState -> ScoopState
  insertSymbol x state 
    = ScoopState newSymbols (dimensions state)
      where newSymbols = M.insert (E.name x) (E.sym x) (symbols state)

  insertDim :: String -> ScoopState -> ScoopState
  insertDim s state 
    = ScoopState (symbols state) newDimensions
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

  -- binApply and createStatement can probably be combined... not sure how yet.
  binApply :: String -> (E.Expr -> E.Expr -> Expression E.Expr) -> Expression E.Symbol -> Expression E.Symbol -> Expression E.Symbol
  binApply s f x y = do
    xsym <- x -- extract symbol
    ysym <- y -- extract symbol
    xexp <- express xsym -- extract expression from symbol
    yexp <- express ysym -- extract expression from symbol
    result <- f xexp yexp -- apply the function
    if(isValidArgs [xexp,yexp]) then
      return (E.ESym result)
    else
      fail $ "Binary operation \'" ++ s ++ "\' has incompatible argument sizes." -- doesn't say which arg it is...

  unApply :: String -> (E.Expr -> Expression E.Expr) -> Expression E.Symbol -> Expression E.Symbol
  unApply s f x = do
    xsym <- x
    xexp <- express xsym
    result <- f xexp
    if(isValidArgs [xexp]) then
      return (E.ESym result)
    else
      fail $ "Unary operation \'" ++ s ++ "\' has incompatible argument sizes."


  -- TODO: this may be duplicate of "binApply"
  createStatement :: (E.Symbol -> E.Symbol -> Expression ()) -> Expression E.Symbol -> Expression E.Symbol -> Expression ()
  createStatement f lhs rhs = do {
    x <- lhs;
    y <- rhs;
    if (isValidArgs [x,y]) then
      f x y
    else
      fail "Dimension mismatch."
  }

  expr :: ScoopParser (Expression E.Symbol)
  expr = buildExpressionParser table term
  

  
  -- a significant difference from the paper is that parameters are also
  -- expressions, so we can multiply two things of the same *type*
  --
  -- TODO/XXX: parsec handles the precedence for me, but i can't force multiply
  -- to be a *unary* function parameterized by the first "term"
  multiply :: Expression E.Symbol -> Expression E.Symbol -> Expression E.Symbol
  multiply p x = do
    xsym <- x
    psym <- p
    
    fail "NO MULYTY. LOLLL!!"
    
    pparam <- parameterize psym -- error message doesn't say which line (noninformative)
    xexp <- express xsym
    result <- primitiveMultiply pparam xexp

    if(E.cols psym == "1" || E.rows xsym == "1" || E.rows xsym == E.cols psym) then
      return (E.ESym result)
    else
      fail "MULTIPLY: Dimension mismatch."

    


  unaryNegate :: Expression E.Symbol -> Expression E.Symbol
  unaryNegate a = unApply "negate" primitiveNegate a

  add :: Expression E.Symbol -> Expression E.Symbol -> Expression E.Symbol
  add a b = binApply "plus" primitiveAdd a b
          
  minus :: Expression E.Symbol -> Expression E.Symbol -> Expression E.Symbol  
  minus a b = binApply "minus" primitiveMinus a b
  
  -- constructors to help build the expression table
  binary name fun assoc 
    = Infix (do{ reservedOp name; return fun }) assoc
  prefix name fun
    = Prefix (do{ reservedOp name; return fun })

  -- parsec doesn't play nice with "mu'*x", since it can't parse
  -- a transpose followed immediately by a multiply
  -- instead, i have it gobble a *single* character (since i know transpose
  -- is a single character operator), and then eat any whitespace following
  postfix name fun
    = Postfix (do { char name; whiteSpace; return fun })  
  
  -- XXX: precedence ordering is *mathematical* precedence (not C-style)
  table = [ --[postfix '\'' scoop_transpose],
            [binary "*" multiply AssocRight],
            [prefix "-" unaryNegate],
            [binary "+" add AssocLeft, 
             binary "-" minus AssocLeft]] 
  
  -- a term is made up of "(expr)", functions thereof, parameters, or 
  -- variables
  term = parens expr
      -- <|> choice (map snd builtinFunctions)
      <|> parameter
      <|> variable
      <|> constant
      -- <|> concatenation
      <?> "term in expression"
  
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
  
  variable :: ScoopParser (Expression E.Symbol)
  variable = do { 
    s <- identifier;
    t <- getState; 
    case (M.lookup s (symbols t)) of
      Just x -> return (do{ return x })
      _ -> fail $ "expected a variable but got " ++ s 
  } <?> "variable"
  
  parameter :: ScoopParser (Expression E.Symbol)
  parameter = do { 
    s <- identifier;
    t <- getState;
    case (M.lookup s (symbols t)) of
      Just x -> return (do{ return x })
      _ -> fail $ "expected a parameter but got " ++ s 
  } <?> "parameter"
  
  constant :: ScoopParser (Expression E.Symbol)
  constant = do {
    s <- naturalOrFloat;
    return (do{ return $ E.sym (either fromIntegral id s) })
  } <?> "constant"
             
  boolOp :: ScoopParser String
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
    

  constraint :: ScoopParser (Expression ())
  constraint = do {
    lhs <- expr;
    p <- boolOp;
    rhs <- expr;

    case (p) of
      "==" -> return $ createStatement scoop_eq lhs rhs
      "<=" -> return $ createStatement scoop_leq lhs rhs
      ">=" -> return $ createStatement scoop_geq lhs rhs

  } <?> "constraint"
  
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
  
  objective :: ScoopParser (Expression ())
  objective = 
    do {
      probSense <- sense;
      obj <- objFunc probSense;
      optionMaybe (reserved "subject to");

      return $ addLine "minimize fakeObj"
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

  defVariable :: ScoopParser (Expression ())
  defVariable = do {
    reserved "variable";
    s <- identifier; 
    size <- optionMaybe shape;
    let (m,n) = (fromMaybe ("1","1") size)
    in if (n == "1") then do
      updateState (insertSymbol (E.Expr s m E.Affine E.Unknown))
      return (addLine $ concat ["variable ", s, "(", m, ")"])
    else
      fail $ "only vector variables are allowed. you attempted to create a matrix variable."
  } <?> "variable"
  
  defParameter :: ScoopParser (Expression ())
  defParameter = do {
    reserved "parameter";
    s <- identifier;
    size <- optionMaybe shape;
    sign <- optionMaybe modifier;
    let (m,n) = fromMaybe ("1","1") size
        p = case (sign) of
          Just E.Positive -> E.Param s m n E.Positive
          Just E.Negative -> E.Param s m n E.Negative
          _ -> E.Param s m n E.Unknown
    in do
      updateState (insertSymbol p)
      return (addLine $ concat ["parameter ", s, "(", m, ",", n, ")", sign_string $ fromMaybe E.Unknown sign]) 
  } <?> "parameter"

  sign_string :: E.Sign -> String
  sign_string E.Unknown = ""
  sign_string E.Positive = " positive"
  sign_string E.Negative = " negative"

  defDimension :: ScoopParser (Expression ())
  defDimension = do {
    reserved "dimension";
    s <- identifier;

    updateState (insertDim s);
    return (addLine $ concat ["dimension ", s])
  } <?> "dimension"

  line :: ScoopParser (Expression ())
  line =  defVariable <|> defParameter <|> defDimension <|> constraint
    <?> "a line (variable, parameter, dimension definition, or a constraint)"

  -- constraint can be affine equality constraint or norm(x) <= t, norm([x;y;z]) <=t or norm(x,y,z) <= t

  scoop_test :: (Expressive a) => a -> (Expression ())
  scoop_test x = do
    xt <- express x
    return ()

    -- addLine (E.name xt)


  scoop_constant :: Double -> Expression E.Expr
  scoop_constant x = do
    t <- newVar "1"

    addLine $ concat [t, " == ", show x]
    --find t
    --subjectTo
    --(Ones 1 1) .* t .== Ones 1 x

    return $ E.Expr t "1" E.Affine E.Unknown

  p1 :: Expression E.Expr
  p1 = scoop_constant 3.0

  p2 :: E.Expr -> Expression E.Expr
  p2 a = 
    scoop_constant 4.0

  -- one file is *one* problem
  -- "minimize", "maximize", or "find" must appear exactly *once*
  cvxProg :: ScoopParser String
  cvxProg = do {
    -- parse the program
    whiteSpace;
    l <- manyTill line (lookAhead objective);
    o <- objective;
    whiteSpace;
    ll <- many line;
    --p <- problem;
    --t <- getState;
    eof;

    -- once the program is parse, we sequence together all the monads and execute
    let f = do {
          -- scoop_test (5.6::Double);
          -- s <- p1;
          -- p2 s;
          -- scoop_test s;
          foldl (>>) (return ()) l; -- "execute" all the lines up to the objective
          o; -- "execute" the objective
          foldl (>>) (return ()) ll; -- "execute" all lines after the objective
        }
        v = St.execState f initState  -- "exectue" and get state
    in return (getString v) -- finally, return only the string (the new program)
  } <?> "optimization problem"



  -- atomic constraints are:
  -- linear (in vars) == param/const
  -- norm(var) <= var
  -- norm(var, var, var, ...) <= var
  -- norm([var; var; var]) <= var


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

