module Parser.CVXDataTypes (Token(..)) where
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
    deriving (Show)
  
  -- data types for annotating the language (TODO)

  
