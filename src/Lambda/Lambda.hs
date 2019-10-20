module Lambda.Lambda
  ( lbeta
  , findNF
  , findNFMax
  , getSteps
  , getStepsMax
  )
where

import           Lambda.Types
import           Lambda.Consts
import qualified Data.Set                      as S
import           Data.Maybe


-- Left Betareduction. 
-- lbeta t = Just t', if t ->b t'
-- lbeta t = Nothing, if no betareduction is possible
lbeta :: Term -> Maybe Term
lbeta (A (L var t1) t2) = Just (subs t1 var t2)
lbeta (A (V var   ) t1) = lbeta t1 >>= \t1' -> return $ A (V var) t1'
lbeta (A t1         t2) = lbeta t1 >>= \t1' -> return $ A t1' t2
lbeta (L var        t1) = lbeta t1 >>= \t1' -> return $ L var t1'
lbeta (V var          ) = Nothing

-- Free Variables
fv :: Term -> S.Set Variable
fv (V var  ) = S.fromList [var]
fv (L var p) = fv p S.\\ S.fromList [var]
fv (A p   q) = S.union (fv p) (fv q)

-- substitution. subs p x n = p[x := n]
subs :: Term -> Variable -> Term -> Term
subs (V y) x n | x == y    = n
               | otherwise = V y
subs (A p q) x n = A (subs p x n) (subs q x n)
subs (L y p) x n
  | y == x = L x p
  | y /= x && (not (S.member y (fv n)) || not (S.member x (fv p))) = L y (subs p x n)
  | otherwise = L z (subs (subs p y (V z)) x n)
  where z = newFV y (S.union (fv p) (fv n))

-- newFV x v
-- generates a variable, that is not in v with a name based on x
newFV :: Variable -> S.Set Variable -> Variable
newFV y s | S.member y' s = newFV y' s
          | otherwise     = y'
  where y' = y ++ "'"

-- betasteps (Just t)
-- generates an infinite list of betareductions
betasteps :: Maybe Term -> [Maybe Term]
betasteps (Just t) = Just t : betasteps t' where t' = lbeta t
betasteps Nothing  = repeat Nothing

-- betatepsToNF t
-- generates a list of betareductions until the term is in normal form
betastepsToNF :: Term -> [Term]
betastepsToNF t = map fromJust $ takeWhile (/= Nothing) $ betasteps (Just t)

-- findNF t
-- returns the normal form of t.
findNF :: Term -> Term
findNF t = last $ betastepsToNF t

-- findNFMax i t
-- returns the normal form of t, but computes only the first i steps.
findNFMax :: Int -> Term -> Term
findNFMax i t = last $ take i $ betastepsToNF t

getArrow :: Bool -> String
getArrow True = arrowUTF8
getArrow False = arrow

-- getSteps sh a t
-- returns a pretty string of all the betareductions to the normal form
-- sh is a formatter function Term -> String
-- a is the string for the arrow. e.g. "->b"
-- t is the term
getSteps :: (Term -> String) -> Bool -> Term -> String
getSteps sh u8 t = sh t
  ++ concatMap (\t -> "\n\t" ++ (getArrow u8) ++ " " ++ sh t) (safetail $ betastepsToNF t)

-- getStepsMax sh a i t
-- like getSteps, but computes only the first i steps
getStepsMax :: (Term -> String) -> Bool -> Int -> Term -> String
getStepsMax sh u8 i t = sh t ++ concatMap
  (\t -> "\n\t" ++ (getArrow u8) ++ " " ++ sh t)
  (safetail $ take i $ betastepsToNF t)


safetail [] = []
safetail xs = tail xs
