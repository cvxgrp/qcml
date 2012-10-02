module Parser.CVX (Token(..),cvxParse, cvxLex) where
  import Parser.CVXDataTypes
  
  -- an Expression is just a list of tokens
  --type Expression = [Token]
  
  -- actually, eval should return an Expression tree (or [] if none) in RPN
  --  eval :: Maybe [Token] -> Result
  -- a Result is either Executed (e.g., Assign, literal operations, etc.)
  --  or an Expression [Matrix "A" (5,5) [PSD, DIAGONAL], Matrix "x" (5,1) [], Multiply]
  --  or a Problem, (objExpr, [constraintExpr])
  --
  -- a single line like A*x + b 
  --   
  --
  -- parse should check syntax and shapes(?) and returns a list of tokens(?) in RPN
  --   parse :: [Token] -> Maybe [Token]
  -- returns Nothing on error?
  --
  --
  -- the expression tree should be [objExpr, [constraintExpr]]
  -- the objExpr and constraintExpr store the Expression in RPN
  -- Expression trees are just a *list* of tokens which we'll store in RPN notation
  
  -- having the parser output RPN is pretty cute, but i've got more
  -- functional code for AST
  cvxParse :: String -> [Token]
  cvxParse s = cvxLex s
  
  --
  -- parameter NAME (sign)
  -- variable NAME
  -- minimize EXPRESSION s.t EXPRESSION == EXPRESSION, EXPRESSION <= EXPRESSION, EXPRESSION >= EXPRESSION
  --    need some way to input "constraints"
  -- everything else is a "problem"
  
  -- parse will check the input language grammar and produce a "problem 
  -- tree"
  --
  -- i.e., it will parse an expression like
  --   square(x) + 1
  -- as [obj=[BinaryOp plusFunc (UnaryOp square (Leaf x)) (Leaf 1)], constraints=[]]
  -- the keyword "subject to" pushes expressions in to the constraint stack
  -- the keyword "minimize" forces a rewrite on the problem
  -- need a shunting yard algorithm to convert tokens to expressions
  
  cvxLex :: String -> [Token]
  cvxLex s = lexRecursively "" s

  lexRecursively :: String -> String -> [Token]
  lexRecursively "+" next = Plus:(lexRecursively "" next)
  lexRecursively "-" next = Subtract:(lexRecursively "" next)
  lexRecursively "*" next = Multiply:(lexRecursively "" next)
  lexRecursively "/" next = Divide:(lexRecursively "" next)
  lexRecursively "==" next = Equals:(lexRecursively "" next)
  lexRecursively "<=" next = LessThanEquals:(lexRecursively "" next)
  lexRecursively "<" (c:next) = lexRecursively ("<"++[c]) next  -- eat
  lexRecursively ">=" next = GreaterThanEquals:(lexRecursively "" next)
  lexRecursively ">" (c:next) = lexRecursively (">"++[c]) next  -- eat
  lexRecursively "(" next = LeftParen:(lexRecursively "" next)
  lexRecursively ")" next = RightParen:(lexRecursively "" next)
  lexRecursively "," next = Comma:(lexRecursively "" next)
  lexRecursively "=" (c:next) 
    | c =='='   = lexRecursively ("="++[c]) next  -- lookahead in case "=="
    | otherwise = Assign:(lexRecursively [c] next)
  -- handle dangling ='s
  lexRecursively "=" "" = [Assign]
  -- skip spaces
  lexRecursively " " next = lexRecursively "" next
  -- end of line
  lexRecursively cur "" = gobble cur
  -- handle case with singleton at the end
  lexRecursively cur [c]
    | isDelimiter c ' ' = gobble cur ++ lexRecursively [c] ""
    | otherwise = lexRecursively (cur ++ [c]) ""
  -- common case is to eat a character
  lexRecursively cur (c:next)
    -- lookahead two characters to determine if we should gobble current
    | isDelimiter c (head next) = gobble cur ++ lexRecursively [c] next
    -- otherwise, just eat a character
    | otherwise = lexRecursively (cur++[c]) next
    
  -- let's the left-right parser know when to gobble its previous characters
  isDelimiter :: Char -> Char -> Bool
  isDelimiter ' ' _   = True
  isDelimiter '+' _   = True
  isDelimiter '-' _   = True
  isDelimiter '*' _   = True
  isDelimiter '/' _   = True
  isDelimiter '=' _   = True
  isDelimiter '<' '=' = True
  isDelimiter '>' '=' = True
  isDelimiter '(' _   = True
  isDelimiter ')' _   = True
  isDelimiter ',' _   = True
  isDelimiter _ _     = False
  
  gobble :: String -> [Token]
  gobble s = case (gobble' s) of
    Just x  -> [x]
    Nothing -> []
  
  gobble' :: String -> Maybe Token
  gobble' "" = Nothing
  gobble' "minimize" = Just Minimize
  gobble' "subjectTo" = Just SubjectTo
  gobble' "parameter" = Just Parameter
  gobble' "variable" = Just Variable
  gobble' s = case reads s :: [(Double,String)] of
    [] -> Just $ Identifier s
    [(a,"")] -> Just $ Literal a
    [(a, b)] -> Just $ Identifier s
    xs -> Nothing


  --insertInSymbolTable :: Token -> [(String, String)]
  --insertInSymbolTable Identifier s = 