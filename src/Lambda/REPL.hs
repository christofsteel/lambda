module Lambda.REPL (
    runRepl
    ) where

import Data.List
import qualified Text.Read as TR

import System.IO
import System.Exit
import Data.Maybe
import Data.Char
import Control.Monad.State.Strict
import System.FilePath.Posix
import System.Console.Haskeline
import System.Console.Haskeline.Completion
import Control.Exception as E

import Lambda.Types
import Lambda.Program
import Lambda.ParserHelper

completableWords = ["let", "print", "printLn", "printT", "traceNF", "traceNFMax", "printNF", "import", "set", "unset"]

isCIPrefixOf n m = map toUpper n `isPrefixOf` map toUpper m

runRepl :: FilePath -> String -> String -> Bool -> IO ()
runRepl importPath arr l ex = evalStateT (runInputT lambdaSettings runStateRepl) 
    (PState { binders = []
            , importPath = importPath
            , arrowSymbol = arr
            , lambdaSymbol = l
            , explicitParen = ex })

searchFunction :: String -> String -> StateT PState IO [Completion]
searchFunction revfirst = searchFunction' ((unwords.words.reverse) revfirst)

searchFunction' :: String -> String -> StateT PState IO [Completion]
searchFunction' pre start = case pre of
                              "import" -> listFiles start
                              "set" -> return $ map simpleCompletion $ filter (start `isCIPrefixOf`) ["ASCII", "EXPLICIT"]
                              "unset" -> return $ map simpleCompletion $ filter (start `isCIPrefixOf`) ["ASCII", "EXPLICIT"]
                              pre -> do
                                                   state <- get
                                                   let names = completableWords ++ map fst (binders state)
                                                   return $ map simpleCompletion $ filter (start `isPrefixOf`) names


lambdaComplete :: CompletionFunc (StateT PState IO)
lambdaComplete = completeWordWithPrev Nothing " \t" searchFunction

lambdaSettings :: Settings (StateT PState IO)
lambdaSettings = Settings { historyFile = Nothing, autoAddHistory = True,
                                   complete = lambdaComplete }

trim = unwords.words

parseCommand :: String -> Command
parseCommand line = case TR.readEither $ trim line of
                   Left x -> case TR.readEither $ "printT " ++ trim line of
                               Left err -> Print $ x ++ " " ++ trim line ++ "'"
                               Right c -> c
                   Right c -> c

runStateRepl :: InputT (StateT PState IO) ()
runStateRepl = do
        state <- lift get
        line <- getInputLine "LAMBDA> "
        case line of
          Nothing -> return ()
          Just "exit" -> return ()
          Just line -> 
                case parseCommand line of
                  (Import f) -> do
                       contentEx <- liftIO $ try $ readFile $ importPath state </> f
                       case contentEx of
                         Left ex -> liftIO (print (ex :: IOError)) >> runStateRepl
                         Right content -> do
                             let prog = read $ replace '\n' ';' $ init content
                             let rememberIPath = importPath state
                             lift $ put (state { importPath = takeDirectory $ importPath state </> f})
                             lift $ runIO prog
                             state <- lift get
                             lift $ put (state { importPath = rememberIPath })
                             runStateRepl
                  command -> do
                      output <- lift $ liftId $ runStep command
                      liftIO $ printMaybe output
                      liftIO $ putStrLn ""
                      runStateRepl
                              
                          
