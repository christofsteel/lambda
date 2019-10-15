module Main (main) where

import Lambda.ParserHelper
import Lambda.Program
import Lambda.REPL
import Lambda.Consts
import Lambda.Types

import System.Environment
import System.Exit
import System.Directory
import System.FilePath.Posix

import Control.Monad.State

printUsage = do
    programName <- getProgName
    putStrLn $ "USAGE: " ++ programName ++ " -h"
    putStrLn $ "       " ++ programName ++ " [-e] [-a] -f FILE"
    putStrLn $ "       " ++ programName ++ " [-e] [-a] PROGRAM"
    putStrLn $ "       " ++ programName ++ " -r [-e] [-a]"
    putStrLn   "Interprets a program in a simple lambda calculus language."
    putStrLn   ""
    putStrLn   "  -h, --help        shows this help."
    putStrLn   "  -e, --explicit    use explicit parentheses in output."
    putStrLn   "  -a, --ascii       use ascii letters to emulate lambda and beta characters."
    putStrLn   "  -f, --file        interpret the contents of FILE instead of PROGRAM."
    putStrLn   "  -r, --repl        opens a REPL."
    putStrLn   "  PROGRAM           a series of COMMANDs, seperated by ';' or a linebreak."
    putStrLn   ""
    putStrLn   "  COMMANDs:"
    putStrLn   "    import FILE          imports and executes a FILE."
    putStrLn   "    print STRING         prints STRING."
    putStrLn   "    printLn STRING       prints STRING, followed by a linebreak."
    putStrLn   "    let VAR = TERM       lets VAR be TERM in all subsequent TERMs."
    putStrLn   "    step VAR             executes beta reduction on the term stored in VAR and"
    putStrLn   "                         stores the resuting term in VAR"
    putStrLn   "    printT TERM          prints term TERM."
    putStrLn   "    printNF TERM         prints the beta normal form for TERM. If it has none,"
    putStrLn   "                         calculates indefinitely."
    putStrLn   "    traceNF TERM         prints all (left) beta reductions for TERM, until the"
    putStrLn   "                         term is in beta normal form."
    putStrLn   "    traceNFMax INT TERM  like traceNF, but computes only the first INT steps."
    putStrLn   "    set OPTION           sets OPTION, where OPTION can be one of [ASCII, EXPLICIT]."
    putStrLn   "    unset OPTION         unsets OPTION, where OPTION can be one of [ASCII, EXPLICIT]."
    putStrLn   ""
    putStrLn   "  TERM:             TERM can be an ABSTRACTION, an APPLICATION or a VAR."
    putStrLn   "    ABSTRACTION:    A term in the form '\\[VAR..].TERM', were [VAR..] are"
    putStrLn   "                    multiple VARs seperated by at least one space. Instead"
    putStrLn   "                    of the character '\\', one can use a unicode lambda."
    putStrLn   "    APPLICATION:    A term in the form '[TERM..]', where [TERM..] are"
    putStrLn   "                    multiple TERMs seperated by at least one space."
    putStrLn   "    VAR:            VAR can be any string composed of letters, digits, the"
    putStrLn   "                    character ' and the character _"
    putStrLn   ""
    putStrLn   "Example:"
    putStrLn $ "  " ++ programName ++ " -e -a \"let I=\\x.x;let K=\\x y.x;let o=\\x.x x;let O=o o;traceNF K I O\""
    
t1 = "let I=\\x.x;let K=\\x y.x;let o=\\x.x x;let O=o o;traceNF K I O"

main = do
    args <- getArgs
    if "-h" `elem` args || "--help" `elem` args || null args
      then printUsage >> exitSuccess
      else do 
        let arr = if "-a" `elem` args || "--ascii" `elem` args

                    then arrow
                    else arrowUTF8
        let lamb = if "-a" `elem` args || "--ascii" `elem` args
                    then lambda
                    else lambdaUTF8
        let ex = "-e" `elem` args || "--explicit" `elem` args
        if "-r" `elem` args || "--repl" `elem` args 
           then do 
               relimportPath <- getCurrentDirectory                    
               importPath <- makeAbsolute relimportPath
               runRepl importPath arr lamb ex
           else do
               (progStr, importPath) <- if "-f" `elem` args || "--file" `elem` args
                             then do
                                 content <- readFile $ last args
                                 importPath <- makeAbsolute  $ takeDirectory $ last args 
                                 return (replace '\n' ';' $ init content, importPath)
                             else do
                                 relimportPath <- getCurrentDirectory                    
                                 importPath <- makeAbsolute relimportPath
                                 return (replace '\n' ';' $ last args, importPath)
               let prog = read progStr
               run importPath arr lamb ex prog

