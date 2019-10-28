module Main
  ( main
  )
where

import           Lambda.ParserHelper
import           Lambda.Program
import           Lambda.REPL
import           Lambda.Consts
import           Lambda.Types

import           System.Environment
import           System.Exit
import           System.Directory
import           System.FilePath.Posix

import           Control.Monad.State

printUsage = do
  programName <- getProgName
  mapM_
    putStrLn
    [ "USAGE: " ++ programName ++ " -h"
    , "       " ++ programName ++ " [-e] [-a] -f FILE"
    , "       " ++ programName ++ " [-e] [-a] PROGRAM"
    , "       " ++ programName ++ " [-e] [-a] -r"
    , "Interprets a program in a simple lambda calculus language."
    , ""
    , "  -h, --help        shows this help."
    , "  -e, --explicit    use explicit parentheses in output."
    , "  -u, --utf8        use ascii letters to emulate lambda and beta characters."
    , "  -f, --file        interpret the contents of FILE instead of PROGRAM."
    , "  -r, --repl        opens a REPL."
    , "  PROGRAM           a series of COMMANDs, seperated by ';' or a linebreak. (See Readme.md for Commands)"
    , ""
    , "Example:"
    , "  "
    ++ programName
    ++ " -e -u \"let I=\\x.x;let K=\\x y.x;let o=\\x.x x;let O=o o;traceNF K I O\""
    ]

t1 = "let I=\\x.x;let K=\\x y.x;let o=\\x.x x;let O=o o;traceNF K I O"

main = do
  args <- getArgs
  if "-h" `elem` args || "--help" `elem` args || null args
    then printUsage >> exitSuccess
    else do
      let u8 = "-u" `elem` args || "--utf8" `elem` args
      let ex = "-e" `elem` args || "--explicit" `elem` args
      if "-r" `elem` args || "--repl" `elem` args
        then do
          relimportPath <- getCurrentDirectory
          importPath    <- makeAbsolute relimportPath
          runRepl importPath u8 ex
        else do
          (progStr, importPath) <- if "-f" `elem` args || "--file" `elem` args
            then do
              content    <- readFile $ last args
              importPath <- makeAbsolute $ takeDirectory $ last args
              return (replace '\n' ';' $ init content, importPath)
            else do
              relimportPath <- getCurrentDirectory
              importPath    <- makeAbsolute relimportPath
              return (replace '\n' ';' $ last args, importPath)
          let prog = read progStr
          run importPath u8 ex prog

