module Lambda.Program
  ( run
  , runStep
  , runIO
  , liftId
  , printMaybe
  , PState(..)
  )
where
import           Lambda.Types
import           Lambda.Lambda
import           Lambda.ParserHelper
import           Lambda.Consts

import           System.Exit
import           Data.Maybe
import qualified Data.Map.Strict               as M
import           Control.Monad.State.Strict
import           Data.Functor.Identity
import           System.FilePath.Posix
import           Data.Char

data PState = PState { binders :: [(Variable, Term)],
                       importPath :: FilePath,
                       arrowSymbol :: String,
                       lambdaSymbol :: String,
                       explicitParen :: Bool } deriving (Show)


run :: FilePath -> String -> String -> Bool -> Prog -> IO ()
run importPath arr l ex p = evalStateT
  (runIO p)
  (PState { binders       = []
          , importPath    = importPath
          , arrowSymbol   = arr
          , lambdaSymbol  = l
          , explicitParen = ex
          }
  )

sh state = lshow (explicitParen state) (lambdaSymbol state)

runStep :: Command -> State PState (Maybe String)
runStep (Let v t) = do
  state <- get
  put (state { binders = (v, t) : binders state })
  return Nothing
runStep (PrintLn t) = return (Just $ t ++ "\n")
runStep (Print   t) = return (Just t)
runStep (PrintT  t) = do
  state <- get
  return $ Just $ sh state $ applyLets t (binders state)
runStep (PrintNF t) = do
  state <- get
  return $ Just $ sh state $ findNF $ applyLets t (binders state)
runStep (TraceNF t) = do
  state <- get
  return
    $  Just
    $  getSteps (sh state) (arrowSymbol state) (applyLets t (binders state))
    ++ "\n"
runStep (TraceNFMax i t) = do
  state <- get
  return
    $ Just
    $ getStepsMax (sh state) (arrowSymbol state) i (applyLets t (binders state))
    ++ "\n"
runStep (Step v) = do
  state <- get
  case lbeta $ applyLets (V v) (binders state) of
    Just t  -> put (state { binders = (v, t) : binders state })
    Nothing -> return ()
  return Nothing
runStep (Set option) = do
  state <- get
  case map toUpper option of
    "ASCII" -> do
      put (state { lambdaSymbol = lambda, arrowSymbol = arrow })
      return $ Just "ASCII mode enabled"
    "EXPLICIT" -> do
      put (state { explicitParen = True })
      return $ Just "EXPLICIT mode enabled"
    _ -> return $ Just "No such Option"
runStep (Unset option) = do
  state <- get
  case map toUpper option of
    "ASCII" -> do
      put (state { lambdaSymbol = lambdaUTF8, arrowSymbol = arrowUTF8 })
      return $ Just "ASCII mode disabled"
    "EXPLICIT" -> do
      put (state { explicitParen = False })
      return $ Just "EXPLICIT mode disabled"
    _ -> return $ Just "No such Option"


printMaybe :: Maybe String -> IO ()
printMaybe Nothing  = return ()
printMaybe (Just s) = putStr s

liftId :: Monad m => StateT s Identity a -> StateT s m a
liftId = mapStateT (return . runIdentity)

runIO :: Prog -> StateT PState IO ()
runIO []              = return ()
runIO (Import f : xs) = do
  state   <- get
  progStr <- liftIO $ do
    content <- readFile $ importPath state </> f
    return $ replace '\n' ';' $ init content
  let prog = read progStr
  put (state { importPath = takeDirectory $ importPath state </> f })
  runIO $ prog ++ xs
runIO (x : xs) = do
  state  <- get
  output <- liftId $ runStep x
  liftIO $ printMaybe output
  runIO xs

-- This is not the most efficient way, but I thought it was nice to let
-- the lambda calculus solve its variables by itself
applyLets :: Term -> [(Variable, Term)] -> Term
applyLets = foldl (\acc (var, term) -> fromJust $ lbeta $ A (L var acc) term)
