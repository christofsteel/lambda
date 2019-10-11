module Lambda.Program (
    run,
    runStep,
    runIO,
    liftId,
    printMaybe
)
where
import Lambda.Types
import Lambda.Lambda

import System.Exit
import Data.Maybe
import qualified Data.Map.Strict as M
import Control.Monad.State
import Data.Functor.Identity

run :: String -> (Term -> String) -> Prog -> IO()
run arr sh p = evalStateT (runIO arr sh p) []
--run = run' []

runStep :: String -> (Term -> String) -> Command -> State [(Variable, Term)] (Maybe String)
runStep arr sh (Let v t) = do
            binds <- get
            put ((v, t):binds)
            return Nothing
runStep arr sh (PrintLn t) = return (Just $ t ++ "\n")
runStep arr sh (Print t) = return (Just t)
runStep arr sh (PrintT t) = do
            binds <- get
            return $ Just $ sh $ applyLets t binds
runStep arr sh (PrintNF t) = do
            binds <- get
            return $ Just $ sh $ findNF $ applyLets t binds
runStep arr sh (TraceNF t) = do
            binds <- get
            return $ Just $ (getSteps sh arr $ applyLets t binds) ++ "\n"
runStep arr sh (TraceNFMax i t) = do
            binds <- get
            return $ Just $ (getStepsMax sh arr i $ applyLets t binds) ++ "\n"

printMaybe :: Maybe String -> IO ()
printMaybe Nothing = return ()
printMaybe (Just s) = putStr s

liftId :: Monad m => StateT s Identity a -> StateT s m a
liftId = mapStateT (return.runIdentity) 

runIO :: String -> (Term -> String) -> Prog -> StateT [(Variable, Term)] IO ()
runIO arr sh [] = liftIO exitSuccess 
runIO arr sh (x:xs) = do
    state <- get
    output <- liftId $ runStep arr sh x
    liftIO $ printMaybe output
    runIO arr sh xs

run' :: [(Variable, Term)] -> String -> (Term -> String) -> Prog -> IO ()
run' _ a sh [] = exitSuccess
run' lets a sh (Let v t:prog) = run' ((v, t):lets) a sh prog
run' lets a sh (PrintLn t:prog) = do
            putStrLn t
            run' lets a sh prog
run' lets a sh (Print t:prog) = do
            putStr t
            run' lets a sh prog
run' lets a sh (PrintT t:prog) = do
            putStr $ sh $ applyLets t lets 
            run' lets a sh prog
run' lets a sh (PrintNF t:prog) = do 
            putStr $ sh $ findNF $ applyLets t lets 
            run' lets a sh prog
run' lets a sh (TraceNF t:prog) = do 
            putStrLn $ getSteps sh a $ applyLets t lets 
            run' lets a sh prog
run' lets a sh (TraceNFMax i t:prog) = do 
            putStrLn $ getStepsMax sh a i $ applyLets t lets 
            run' lets a sh prog
    

-- This is not the most efficient way, but I thought it was nice to let
-- the lambda calculus solve its variables by itself
applyLets :: Term -> [(Variable, Term)] -> Term
applyLets = foldl (\acc (var, term) -> fromJust $ lbeta $ A (L var acc) term)
