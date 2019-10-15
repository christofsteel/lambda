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
    , "       " ++ programName ++ " -r [-e] [-a]"
    , "Interprets a program in a simple lambda calculus language."
    , ""
    , "  -h, --help        shows this help."
    , "  -e, --explicit    use explicit parentheses in output."
    , "  -a, --ascii       use ascii letters to emulate lambda and beta characters."
    , "  -f, --file        interpret the contents of FILE instead of PROGRAM."
    , "  -r, --repl        opens a REPL."
    , "  PROGRAM           a series of COMMANDs, seperated by ';' or a linebreak."
    , ""
    , "  COMMANDs:"
    , "    import FILE          imports and executes a FILE."
    , "    print STRING         prints STRING."
    , "    printLn STRING       prints STRING, followed by a linebreak."
    , "    let VAR = TERM       lets VAR be TERM in all subsequent TERMs."
    , "    step VAR             executes beta reduction on the term stored in VAR and"
    , "                         stores the resuting term in VAR"
    , "    printT TERM          prints term TERM."
    , "    printNF TERM         prints the beta normal form for TERM. If it has none,"
    , "                         calculates indefinitely."
    , "    traceNF TERM         prints all (left) beta reductions for TERM, until the"
    , "                         term is in beta normal form."
    , "    traceNFMax INT TERM  like traceNF, but computes only the first INT steps."
    , "    set OPTION           sets OPTION, where OPTION can be one of [ASCII, EXPLICIT]."
    , "    unset OPTION         unsets OPTION, where OPTION can be one of [ASCII, EXPLICIT]."
    , ""
    , "  TERM:             TERM can be an ABSTRACTION, an APPLICATION or a VAR."
    , "    ABSTRACTION:    A term in the form '\\[VAR..].TERM', were [VAR..] are"
    , "                    multiple VARs seperated by at least one space. Instead"
    , "                    of the character '\\', one can use a unicode lambda."
    , "    APPLICATION:    A term in the form '[TERM..]', where [TERM..] are"
    , "                    multiple TERMs seperated by at least one space."
    , "    VAR:            VAR can be any string composed of letters, digits, the"
    , "                    character ' and the character _"
    , ""
    , "Example:"
    , "  "
    ++ programName
    ++ " -e -a \"let I=\\x.x;let K=\\x y.x;let o=\\x.x x;let O=o o;traceNF K I O\""
    ]

t1 = "let I=\\x.x;let K=\\x y.x;let o=\\x.x x;let O=o o;traceNF K I O"

main = do
  args <- getArgs
  if "-h" `elem` args || "--help" `elem` args || null args
    then printUsage >> exitSuccess
    else do
      let
        arr =
          if "-a" `elem` args || "--ascii" `elem` args then arrow else arrowUTF8
      let
        lamb = if "-a" `elem` args || "--ascii" `elem` args
          then lambda
          else lambdaUTF8
      let ex = "-e" `elem` args || "--explicit" `elem` args
      if "-r" `elem` args || "--repl" `elem` args
        then do
          relimportPath <- getCurrentDirectory
          importPath    <- makeAbsolute relimportPath
          runRepl importPath arr lamb ex
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
          run importPath arr lamb ex prog

