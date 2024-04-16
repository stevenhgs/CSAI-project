import Data.Map

import Control.Monad.State
import Control.Monad.Reader

data Term = Var String | Con Int | Add Term Term

 -- environment that maps variable names to their value
type Env = Map String Int

evalS :: Term -> State Env Int
evalS (Var x)   = gets (! x)
evalS (Con a)   = return a
evalS (Add t u) = 
  do x <- evalS t
     y <- evalS u
     return (x+y)


-- * HOMEWORK 5B
--
--   rewrite evalS to use the `Applicative` methods
--   rather than the `Monad` methods (return, >>=).

evalS' :: Term -> State Env Int
evalS' (Var x)   = gets (! x)
evalS' (Con a)   = pure a
evalS' (Add t u) = (+) <$> evalS' t <*> evalS' u
