module Lambda.Types.Term
  ( Variable
  , Term(L, A, V)
  , lshow
  , readTerm
  , readVar
  )
where

import           Lambda.ParserHelper
import           Lambda.Consts
import           Data.Char

type Variable = String
data Term = L Variable Term | A Term Term | V Variable deriving Eq
instance Show Term where
  show = explShow' False

lshow True  = explShow'
lshow False = minShow'

getLambda :: Bool -> String
getLambda True = lambdaUTF8
getLambda False = lambda

-- explShow' l t
-- returns an explicitly parenthesized string of the term t. The lambda sign is l
explShow' :: Bool -> Term -> String
explShow' u8 (L var t ) = "(" ++ (getLambda u8) ++ var ++ "." ++ explShow' u8 t ++ ")"
explShow' u8 (A t1  t2) = "(" ++ explShow' u8 t1 ++ " " ++ explShow' u8 t2 ++ ")"
explShow' u8 (V var   ) = var

-- minShow' l t
-- retuns a minified string of the term t. The lambda sign is l
-- minifications:
-- (a) => a
-- (a a) a => a a a
-- la.lb.c => la b.c
-- a (lb.c) => a lb.c
-- a lb.(c d) => a lb.c d
minShow' :: Bool -> Term -> String
minShow' u8 (L var (L var' t)) = minShow' u8 (L (var ++ " " ++ var') t)
minShow' u8 (L var t         ) = (getLambda u8) ++ var ++ "." ++ minShow' u8 t
minShow' u8 (A t1@(A t1' t2'@(L v t')) t2@(A t1'' t2'')) =
  "(" ++ minShow' u8 t1 ++ ") (" ++ minShow' u8 t2 ++ ")"
minShow' u8 (A t1@(A t1' t2'@(L v t')) t2) =
  "(" ++ minShow' u8 t1 ++ ") " ++ minShow' u8 t2
minShow' u8 (A t1@(A t1' t2') t2@(A t1'' t2'')) =
  minShow' u8 t1 ++ " (" ++ minShow' u8 t2 ++ ")"
minShow' u8 (A t1@(A t1' t2') t2) = minShow' u8 t1 ++ " " ++ minShow' u8 t2
minShow' u8 (A t1@(V v) t2@(V v2)) = minShow' u8 t1 ++ " " ++ minShow' u8 t2
minShow' u8 (A t1@(V v) t2@(L v2 t2')) = minShow' u8 t1 ++ " " ++ minShow' u8 t2
minShow' u8 (A t1@(V v) t2) = minShow' u8 t1 ++ "(" ++ minShow' u8 t2 ++ ")"
minShow' u8 (A t1@(L v t) t2@(A t1' t2')) =
  "(" ++ minShow' u8 t1 ++ ") (" ++ minShow' u8 t2 ++ ")"
minShow' u8 (A t1@(L v t) t2) = "(" ++ minShow' u8 t1 ++ ") " ++ minShow' u8 t2
minShow' u8 (V var          ) = var

-- Grammar
--
-- term = <lambda> | <appl> | <var>
-- lambda = "\" <lvar> "." <term> 
-- lambda = "(" <lambda> ")"
-- lvar = <var>
-- lvar = <var> " " <lvar>
-- lvar = "(" <lvar> ")"
-- var = V
-- var = "(" <var> ")"
-- appl1 = "(" <lambda> ")" <appl2>
-- appl1 = "(" <appl1> ")" <appl2>
-- appl1 = <var> <appl2>
-- appl1 = "(" <appl1> ")"
-- appl2 = " " "(" <appl1> ")" <appl2>
-- appl2 = " " "(" <lambda> ")" <appl2>
-- appl2 = " " <var> <appl2>
-- appl2 = " " "(" <appl1> "):
-- appl2 = " " <lambda>
-- appl2 = " " <var>
-- parses a lambda term by the grammar given above.
instance Read Term where
  readsPrec d = readTerm

readTerm r = readVar r ++ readAppl1 r ++ readLambda r

readLambda r = readLambda1 r ++ readParen' readLambda r

readLambda1 r = do
  (_   , s) <- readOnlyChars "λ\\" r
  (lvar, t) <- readLvar s
  ('.' , u) <- readWhiteSpaces readChar t
  (var , v) <- readTerm u
  return (foldr L (L (last lvar) var) $ init lvar, v) -- binds to the right

readLvar s =
  do
      (V var, t) <- readVar s
      return ([var], t)
    ++ do
         (V var, t) <- readVar s
         (vars , u) <- readWhiteSpaces1 readLvar t
         return (var : vars, u)
    ++ readParen' readLvar s

readAppl1 r =
  readAppl11 (readParen' readLambda) r
    ++ readAppl11 (readParen' readAppl1) r
    ++ readAppl11 readVar                r
    ++ readParen' readAppl1 r

readAppl11 reader r = do
  (term , s) <- reader r
  (appls, t) <- readAppl2 s
  return (foldl A term appls, t) -- binds to the left

readAppl2 r =
  readAppl21 (readParen' readLambda) r
    ++ readAppl21 (readParen' readAppl1) r
    ++ readAppl21 readVar                r
    ++ readAppl22 readLambda             r
    ++ readAppl22 (readParen' readAppl1) r
    ++ readAppl22 readVar                r

readAppl21 reader r = do
  (term , t) <- readWhiteSpaces1 reader r
  (appl2, u) <- readAppl2 t
  return (term : appl2, u)
readAppl22 reader r = do
  (term, t) <- readWhiteSpaces1 reader r
  return ([term], t)

readVar r = readVar1 r ++ readParen' readVar r
readVar1 r = do
  (var, s) <- lex r
  if not (validVariable var) then fail "No Variable" else return (V var, s)
validVariable = all (\d -> isAlphaNum d || d == '\'' || d == '_')
