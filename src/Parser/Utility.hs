module Parser.Utility (ScoopState(..), insertVariable, insertParameter, insertDim, binApply, unApply, listApply, boolApply) where
  import Expression.Expression
  import Atoms.Common

  import Control.Monad(foldM)
  import qualified Data.Map as M
  import qualified Data.Set as S

  -- contains state information for parser
  data ScoopState = ScoopState {
    variables :: M.Map String Expr, -- vars or parameters
    parameters :: M.Map String Param,
    dimensions :: S.Set String
  }

  insertVariable :: Expr -> ScoopState -> ScoopState
  insertVariable x state = ScoopState newVars (parameters state) (dimensions state)
    where newVars = M.insert (name x) x (variables state)

  insertParameter :: Param -> ScoopState -> ScoopState
  insertParameter x state = ScoopState (variables state) newParams (dimensions state)
    where newParams = M.insert (name x) x (parameters state)

  insertDim :: String -> ScoopState -> ScoopState
  insertDim s state = ScoopState (variables state) (parameters state) newDimensions
    where newDimensions = S.insert s (dimensions state)


  -- binApply and boolApply can probably be combined... not sure how yet.
  binApply :: (Expr -> Expr -> Rewriter Expr) -> Rewriter Symbol -> Rewriter Symbol -> Rewriter Symbol
  binApply f x y = do
    xsym <- x -- extract symbol
    ysym <- y -- extract symbol
    xexp <- express xsym -- extract expression from symbol
    yexp <- express ysym -- extract expression from symbol
    result <- f xexp yexp -- apply the function
    return (ESym result)
  

  unApply :: (Expr -> Rewriter Expr) -> Rewriter Symbol -> Rewriter Symbol
  unApply f x = do
    xsym <- x
    xexp <- express xsym
    result <- f xexp
    return (ESym result)

  listApply :: ([Expr] -> Rewriter Expr) -> [Rewriter Symbol] -> Rewriter Symbol
  listApply f xs = do
    args <- foldr (\x y -> do { exprs <- y; xsym <- x; xexp <- express xsym; return (xexp:exprs) }) (return []) xs
    result <- f args
    return (ESym result)

  --unApplyList :: String -> (Expr -> Rewriter Expr) -> [Rewriter Symbol] -> Rewriter Symbol
  --unApplyList...


  -- TODO: this may be duplicate of "binApply"
  boolApply :: (Symbol -> Symbol -> Rewriter ()) -> Rewriter Symbol -> Rewriter Symbol -> Rewriter ()
  boolApply f lhs rhs = do {
    x <- lhs;
    y <- rhs;
    f x y
  }