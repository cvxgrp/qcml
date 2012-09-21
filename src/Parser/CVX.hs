module Parser.CVX (Token,parse) where
  -- how do we handle atoms?
  data Token = Literal Int  -- only ints are allowed to be literal
    | Identifier String
    | Assign 
    -- grouping
    | LeftParen
    | RightParen
    -- keywords
    | Minimize
    | SubjectTo
    | Parameter
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
    deriving (Show)
  
  -- an Expression is just a list of tokens
  type Expression = [Token]
  
  -- parse should return an Expression tree
  -- Expression trees are just a *list* of tokens which we'll store in RPN notation
  parse :: String -> Expression
  parse s = parseRecursively "" s
  

  parseRecursively :: String -> String -> Expression
  parseRecursively "minimize" next = Minimize:(parseRecursively "" next)  
  parseRecursively "subjectTo" next = SubjectTo:(parseRecursively "" next)
  parseRecursively "parameter" next = Parameter:(parseRecursively "" next)
  parseRecursively "+" next = Plus:(parseRecursively "" next)
  parseRecursively "-" next = Subtract:(parseRecursively "" next)
  parseRecursively "*" next = Multiply:(parseRecursively "" next)
  parseRecursively "/" next = Divide:(parseRecursively "" next)
  parseRecursively "==" next = Equals:(parseRecursively "" next)
  parseRecursively "<=" next = LessThanEquals:(parseRecursively "" next)
  parseRecursively ">=" next = GreaterThanEquals:(parseRecursively "" next)
  parseRecursively "=" (c:next) 
    | c =='='   = parseRecursively ("="++[c]) next  -- lookahead in case "=="
    | otherwise = Assign:(parseRecursively "" next)
  parseRecursively "=" "" = [Assign]  -- a dangling ='s
  parseRecursively "(" next = LeftParen:(parseRecursively "" next)
  parseRecursively ")" next = RightParen:(parseRecursively "" next)
  parseRecursively "," next = Comma:(parseRecursively "" next)
  -- skip spaces
  parseRecursively " " next = parseRecursively "" next
  -- end of line
  parseRecursively cur "" = gobble cur
  -- common case is to eat a character
  parseRecursively cur (c:next)
    -- special case: candidate could be <= or >=
    | cur == "<" || cur == ">" = parseRecursively (cur++[c]) next
    -- special case: nothing left at the end, doesn't end with delimiter
    | next == "" && not (isDelimiter c ' ') = parseRecursively (cur ++ [c]) ""
    -- special case: nothing left at the end, ends with delimiter
    | next == "" && isDelimiter c ' ' = gobble cur ++ parseRecursively [c] ""
    -- lookahead two characters to determine if we should gobble current
    | isDelimiter c (head next) = gobble cur ++ parseRecursively [c] next
    -- otherwise, just eat a character
    | otherwise = parseRecursively (cur++[c]) next
  
  
  -- let's the left-right parser know when to gobble its previous characters
  -- XXX: gobble just means "tell us if it's an ident or a literal"
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
  
  
  gobble :: String -> Expression
  gobble s = case (gobble' s) of
    Just x  -> [x]
    Nothing -> []
  
  gobble' :: String -> Maybe Token
  gobble' "" = Nothing
  gobble' s = case reads s :: [(Int,String)] of
    [] -> Just $ Identifier s
    [(a,"")] -> Just $ Literal a
    [(a, b)] -> Just $ Identifier s
    xs -> Nothing


  --insertInSymbolTable :: Token -> [(String, String)]
  --insertInSymbolTable Identifier s = 