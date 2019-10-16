module Lambda.Program
  ( run
  , runStep
  , runIO
  , liftId
  , printMaybe
  , defaultState 
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
                       reverseLets :: [Variable],
                       reverseNat :: Bool,
                       importPath :: FilePath,
                       arrowSymbol :: String,
                       lambdaSymbol :: String,
                       explicitParen :: Bool } deriving (Show)

defaultState = PState { binders = []
                      , importPath = "."
                      , arrowSymbol = arrowUTF8
                      , lambdaSymbol = lambdaUTF8 
                      , explicitParen = False
                      , reverseLets = []
                      , reverseNat = True
                      }

run :: FilePath -> String -> String -> Bool -> Prog -> IO ()
run importPath arr l ex p = evalStateT
  (runIO p)
  (defaultState { binders       = []
          , importPath    = importPath
          , arrowSymbol   = arr
          , lambdaSymbol  = l
          , explicitParen = ex
          }
  )


runStep :: Command -> State PState (Maybe String)
runStep (Let v t) = do
  state <- get
  put (state { binders = (v, t) : binders state })
  return Nothing
runStep (PrintLn t) = return (Just $ t ++ "\n")
runStep (Print   t) = return (Just t)
runStep (PrintT  t) = do
  term <- applyLets t
  sh <- getShowFunction
  return $ Just $ sh term
runStep (PrintNF t) = do
  term <- applyLets t
  sh <- getShowFunction
  return $ Just $ sh $ findNF term
runStep (TraceNF t) = do
  state <- get
  term <- applyLets t
  sh <- getShowFunction
  return $ Just $ getSteps sh (arrowSymbol state) term ++ "\n"
runStep (TraceNFMax i t) = do
  state <- get
  term <- applyLets t
  sh <- getShowFunction
  return $ Just $ getStepsMax sh (arrowSymbol state) i term ++ "\n"
runStep (Step v) = do
  state <- get
  term <- applyLets (V v)
  case lbeta term of
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
applyLets :: Term -> State PState Term
applyLets t = foldl (\acc (var, term) -> fromJust $ lbeta $ A (L var acc) term) t . binders <$> get

getShowFunction :: State PState (Term -> String)
getShowFunction = do
    state <- get
    return $ lshow (explicitParen state) (lambdaSymbol state)

applyRevLets :: Term -> Term
applyRevLets = id
