{-# LANGUAGE FlexibleInstances #-}
module Lambda.Types.Prog
  ( Prog
  , Command(..)
  )
where

import           Lambda.Types.Term              ( Variable
                                                , readVar
                                                , readTerm
                                                , Term(V)
                                                )
import           Lambda.ParserHelper            ( readOnlyChars
                                                , readWhiteSpaces
                                                , readChar
                                                , readUntilOneOf
                                                )

type Prog = [Command]
data Command = Let Variable Term 
             | Print String 
             | PrintLn String 
             | PrintT Term 
             | PrintNF Term 
             | TraceNF Term 
             | TraceNFMax Int Term 
             | Import String 
             | Step Variable 
             | Set String 
             | Unset String 
             deriving Show

instance {-# OVERLAPPING #-} Read Prog where
  readsPrec d = readProg
   where
    readProg t =
      do
          (command, u) <- readCommand t
          return ([command], u)
        ++ do
             (command, u) <- readCommand t
             (';'    , v) <- readChar u
             (prog   , w) <- readProg v
             return (command : prog, w)
        ++ do
             (';' , v) <- readChar t
             (prog, w) <- readProg v
             return (prog, w)
        ++ do
             (command, u) <- readCommand t
             (';'    , v) <- readChar t
             return ([command], v)
        ++ do
             (';', v) <- readChar t
             return ([], v)
instance Read Command where
  readsPrec d = readCommand

readCommand t =
  readLet t
    ++ readPrint t
    ++ readPrintT t
    ++ readPrintNF t
    ++ readPrintLn t
    ++ readTraceNF t
    ++ readTraceNFMax t
    ++ readImport t
    ++ readStep t
    ++ readSet t
    ++ readUnset t

readLet t = do
  ("let", u) <- lex t
  (V var, v) <- readVar u
  ('='  , w) <- readWhiteSpaces readChar v
  (term , x) <- readWhiteSpaces readTerm w
  return (Let var term, x)

readPrint t = do
  ("print", u) <- lex t
  (line   , v) <- readWhiteSpaces (readUntilOneOf ";") u
  return (Print line, v)

readPrintLn t = do
  ("printLn", u) <- lex t
  (line     , v) <- readWhiteSpaces (readUntilOneOf ";") u
  return (PrintLn line, v)

readPrintT t = do
  ("printT", u) <- lex t
  (term    , v) <- readWhiteSpaces readTerm u
  return (PrintT term, v)

readTraceNF t = do
  ("traceNF", u) <- lex t
  (term     , v) <- readWhiteSpaces readTerm u
  return (TraceNF term, v)

readTraceNFMax t = do
  ("traceNFMax", u) <- lex t
  (max         , v) <- lex u
  (term        , w) <- readWhiteSpaces readTerm v
  return (TraceNFMax (read max) term, w)

readPrintNF t = do
  ("printNF", u) <- lex t
  (term     , v) <- readWhiteSpaces readTerm u
  return (PrintNF term, v)

readImport t = do
  ("import", u) <- lex t
  (file    , v) <- readWhiteSpaces (readUntilOneOf ";") u
  return (Import ((unwords . words) file), v)

readStep t = do
  ("step", u) <- lex t
  (V var , v) <- readVar u
  return (Step var, v)

readSet t = do
  ("set" , u) <- lex t
  (option, v) <- readWhiteSpaces (readUntilOneOf ";") u
  return (Set ((unwords . words) option), v)

readUnset t = do
  ("unset", u) <- lex t
  (option , v) <- readWhiteSpaces (readUntilOneOf ";") u
  return (Unset ((unwords . words) option), v)
