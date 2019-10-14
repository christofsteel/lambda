module Lambda.REPL (
    runRepl
    ) where

import System.IO
import System.Exit
import Data.Maybe
import Control.Monad.State
import System.FilePath.Posix

import Lambda.Types
import Lambda.Program
import Lambda.ParserHelper

runRepl :: FilePath -> String -> (Term -> String) -> IO ()
runRepl importPath arr sh = evalStateT (runStateRepl importPath arr sh) []

runStateRepl :: FilePath -> String -> (Term -> String) -> StateT [(Variable, Term)] IO ()
runStateRepl importPath arr sh = do
        line <- liftIO $ do
            putStr "LAMBDA> " 
            hFlush stdout
            iseof <- isEOF 
            if not iseof
               then getLine
               else exitSuccess
        if line == "" 
           then runStateRepl importPath arr sh
           else do                      
                let command = (fmap fst.listToMaybe.reads) line :: Maybe Command
                case command of
                  Nothing -> liftIO (putStrLn ("Error, did not understand: " ++ line)) >> runStateRepl importPath arr sh
                  Just (Import f) -> do
                       progStr <- liftIO $ do 
                                 content <- readFile $ importPath </> f
                                 return $ replace '\n' ';' $ init content
                       let prog = read progStr
                       let importPath' = takeDirectory  $ importPath </> f
                       runIO importPath' arr sh prog
                       liftIO $ putStrLn ""
                       runStateRepl importPath arr sh
                  Just c -> do
                      output <- liftId $ runStep arr sh c  
                      liftIO $ printMaybe output
                      liftIO $ putStrLn ""
                      runStateRepl importPath arr sh
                              
                          
