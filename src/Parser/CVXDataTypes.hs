module Parser.CVXDataTypes (Token(..)) where
  -- this file is around only to help us keep track of the lexer
  
  -- how do we handle atoms?
  data Token = Literal Double  -- only ints are allowed to be literal
    | Identifier String
    | Assign 
    -- grouping
    | LeftParen
    | RightParen
    -- keywords
    | Minimize
    | SubjectTo
    | Parameter
    | Variable
    -- | Property String -- positive, etc.
    -- | Atom String -- if we encounter an unitialized identifier, could be an atom
    -- operators
    | Plus
    | Multiply
    | Divide
    | Subtract
    -- boolean operators
    | Equals 
    | LessThanEquals 
    | GreaterThanEquals
    -- filler
    | Comma
    deriving (Eq, Show)
  
  -- old lexer
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

  
