module Rewriter.ECOS (isDCP, Node(..)) where
  import qualified Data.Map as M
  import Rewriter.Atoms

  data Node = Var String
    | Const Double
    | BinaryOp String --Monotonicity Monotonicity
    | UnaryOp String --Monotonicity
    deriving (Show)

  type RPNStack = [Node]
  
    
  isDCP :: RPNStack -> Bool
  isDCP expr = case (evalDCPStack expr []) of
    Just _ -> True
    otherwise -> False
  
  evalDCPStack :: RPNStack -> [Expr] -> Maybe Vexity
  evalDCPStack [] expr = case (expr) of
    [x] -> vexity x
    _ -> Nothing
  evalDCPStack (Var s:rest) expr = evalDCPStack rest (Expr Nothing (Just Affine):expr)
  evalDCPStack (Const d:rest) expr = evalDCPStack rest (Constant d:expr)
  evalDCPStack (BinaryOp s:rest) expr = evalDCPStack rest (evalBinaryOp s expr)
  evalDCPStack (UnaryOp s:rest) expr = evalDCPStack rest (evalUnaryOp s expr)
  

  