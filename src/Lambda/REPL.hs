module Lambda.REPL (
    runRepl
    ) where

import System.IO
import System.Exit
import Data.Maybe
import Control.Monad.State

import Lambda.Types
import Lambda.Program

runRepl :: String -> (Term -> String) -> IO ()
runRepl arr sh = evalStateT (runStateRepl arr sh) []

runStateRepl :: String -> (Term -> String) -> StateT [(Variable, Term)] IO ()
runStateRepl arr sh = do
        line <- liftIO $ do
            putStr "LAMBDA> " 
            hFlush stdout
            iseof <- isEOF 
            if not iseof
               then getLine
               else exitSuccess
        if line == "" 
           then runStateRepl arr sh
           else do                      
                let command = (fmap fst.listToMaybe.reads) line :: Maybe Command
                case command of
                  Nothing -> liftIO (putStrLn ("Error, did not understand: " ++ line)) >> runStateRepl arr sh
                  Just c -> do
                      output <- liftId $ runStep arr sh c  
                      liftIO $ printMaybe output
                      runStateRepl arr sh
                              
                          
