module Lambda.Program
  ( run
  , runStep
  , runIO
  , liftId
  , printMaybe
  , defaultState 
  , defaultConfig
  , Config(..)
  , PState(..)
  , optionIsTrue
  , updateConfig
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


data Config = Config { reverseNat :: Bool,
                       explicitParen :: Bool,
                       use_utf8 :: Bool} deriving (Show)

defaultConfig = Config { reverseNat = True
                       , explicitParen = False
                       , use_utf8 = True }

isCIEq :: String -> String -> Bool
isCIEq s t = (map toUpper s) == (map toUpper t)

isFalse :: String -> Bool
isFalse = isCIEq "false"

isTrue :: String -> Bool
isTrue = isCIEq "true"

optionIsTrue :: String -> String -> String -> Bool
optionIsTrue option parseoption value = isCIEq parseoption option && isTrue value

optionIsFalse :: String -> String -> String -> Bool
optionIsFalse option parseoption value = isCIEq parseoption option && isFalse value

updateConfig :: String -> String -> Config -> Either String Config
updateConfig option value config
  | optionIsTrue "nat" option value = Right config {reverseNat = True}  
  | optionIsFalse "nat" option value = Right config {reverseNat = False}  
  | optionIsTrue "utf8" option value = Right config {use_utf8 = True}  
  | optionIsFalse "utf8" option value = Right config {use_utf8 = False}  
  | optionIsTrue "explicit" option value = Right config {explicitParen = True}
  | optionIsFalse "explicit" option value = Right config {explicitParen = False}
  | otherwise = Left $ "No such option or value (" ++ option ++ ", " ++ value ++ ")"

data PState = PState { binders :: [(Variable, Term)],
                       reverseLets :: [Variable],
                       importPath :: FilePath,
                       config :: Config} deriving (Show)

defaultState = PState { binders = []
                      , importPath = "."
                      , config = defaultConfig 
                      , reverseLets = []
                      }

run :: FilePath -> Bool -> Bool -> Prog -> IO ()
run importPath u8 ex p = evalStateT
  (runIO p)
  (defaultState { binders       = []
                , config = defaultConfig { use_utf8 = u8
                                         , explicitParen = ex
                                         }
                , importPath    = importPath
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
runStep (PrintNFMax i t) = do
  term <- applyLets t
  sh <- getShowFunction
  return $ Just $ sh $ findNFMax i term
runStep (TraceNF t) = do
  state <- get
  term <- applyLets t
  sh <- getShowFunction
  return $ Just $ getSteps sh (use_utf8 $ config state) term ++ "\n"
runStep (TraceNFMax i t) = do
  state <- get
  term <- applyLets t
  sh <- getShowFunction
  return $ Just $ getStepsMax sh (use_utf8 $ config state) i term ++ "\n"
runStep (Step v) = do
  state <- get
  term <- applyLets (V v)
  case lbeta term of
    Just t  -> put (state { binders = (v, t) : binders state })
    Nothing -> return ()
  return Nothing
runStep (Set option value) = do
  state <- get
  case updateConfig option value (config state) of
    Left error -> return $ Just error
    Right newconfig -> put (state {config = newconfig}) >> return Nothing

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
    return $ lshow (explicitParen $ config state) (use_utf8 $ config state)

applyRevLets :: Term -> Term
applyRevLets = id
