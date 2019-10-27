{-# LANGUAGE FlexibleInstances #-}

module Lambda.Program
  ( run
  , runStep
  , runIO
  , liftId
  , printMaybe
  , defaultState
  , defaultConfig
  , Config
  , ps1
  , steps
  , PState(..)
  , optionIsTrue
  , updateConfig
  , cn
  , applyNats
  )
where
import           Lambda.Types
import           Lambda.Lambda
import           Lambda.ParserHelper
import           Lambda.Consts

import           System.Exit
import           Data.Maybe
import           Data.List
import qualified Data.Map.Strict               as M
import           Control.Monad.State.Strict
import           Data.Functor.Identity
import           System.FilePath.Posix
import           Data.Char
import qualified Text.Read as TR


class Show c => ConfigValue c where
    readCfg :: String -> Maybe c
    showCfg :: c -> String
    showCfg = show

instance ConfigValue Bool where
    readCfg val
        | isTrue val = Just True
        | isFalse val = Just False
        | otherwise = Nothing

instance ConfigValue String where
    readCfg = Just
    showCfg = id

instance ConfigValue Int where
    readCfg = TR.readMaybe

    {-
data Config = Config { reverseNat :: ConfigValue,
                       explicitParen :: ConfigValue,
                       ps1 :: ConfigValue,
                       use_utf8 :: ConfigValue} deriving (Show)
                       -}
                           {-data Config = Config { reverseNat :: Bool,
                       explicitParen :: Bool,
                       ps1 :: String,
                       use_utf8 :: Bool} deriving (Show)
-}

type Config = M.Map String String
defaultConfig  :: M.Map String String
defaultConfig  = M.fromList [ ("rnat", "False")
                     , ("pnat", "True")
                     , ("explicit", "False")
                     , ("ps1", "LAMBDA")
                     , ("utf8", "True")
                     , ("steps", "100")
                     ]
getCfg :: ConfigValue a => String -> a -> Config -> a
getCfg key def config = let v = M.lookup key config >>= readCfg in
                  fromMaybe def v 

reverseNat, explicitParen, use_utf8 :: Config -> Bool
reverseNat = getCfg "rnat" False
parseNat = getCfg "pnat" True
explicitParen = getCfg "explicit" False
use_utf8 = getCfg "utf8" True
ps1 :: Config -> String
ps1 = getCfg "ps1" "LAMBDA"
steps :: Config -> Int
steps = getCfg "steps" 100

isCIEq :: String -> String -> Bool
isCIEq s t = map toUpper s == map toUpper t

isFalse :: String -> Bool
isFalse = isCIEq "false"

isTrue :: String -> Bool
isTrue = isCIEq "true"

optionIsTrue :: String -> String -> String -> Bool
optionIsTrue option parseoption value = isCIEq parseoption option && isTrue value

optionIsFalse :: String -> String -> String -> Bool
optionIsFalse option parseoption value = isCIEq parseoption option && isFalse value

updateConfig :: ConfigValue a => String -> a-> Config ->  Config
updateConfig k v = M.insert (map toLower k) (showCfg v)

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
                , config = updateConfig "utf8" u8 $ updateConfig "explicit" ex defaultConfig
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
  let newconfig = updateConfig option value (config state)
  put (state {config = newconfig}) >> return Nothing
runStep (Get option) = do
    state <- get
    return $ M.lookup option (config state)
runStep (AddReverse v) = do
    state <- get
    let reverses = nub $ v:reverseLets state
    put (state {reverseLets = reverses}) >> return Nothing
runStep (DelReverse v) = do
    state <- get
    let reverses = filter ((/=) v) $ reverseLets state
    put (state {reverseLets  = reverses}) >> return Nothing
runStep (ShowReverse) = do
    state <- get
    return $ Just $ show $ reverseLets state


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
applyLets t = do
    state <- get
    let t' = if parseNat (config state) then applyNats t else t
    return $ foldl (\acc (var, term) -> fromJust $ lbeta $ A (L var acc) term) t' (binders state)

cn :: Int -> Term
cn 0 = L "s" (L "z" (V "z"))
cn n = case cn (n-1) of
        L "s" (L "z" m) -> L "s" (L "z" (A (V "s") m))


applyNats' :: Term -> [String] -> Term
applyNats' (V c) bound
    | c `elem` bound = V c
    | otherwise = case TR.readMaybe c of
                    Just i -> if i >= 0 then cn i else V c
                    Nothing -> V c
applyNats' (L v p) bound = L v $ applyNats' p (v:bound)
applyNats' (A p q) bound = A (applyNats' p bound) (applyNats' q bound)

applyNats :: Term -> Term
applyNats t = applyNats' t []

getShowFunction :: State PState (Term -> String)
getShowFunction = do
    state <- get
    let rn = reverseNat $ config state
    let wrapper = if rn then \t -> reverseNatTerm t else id
    letsWrapper <- getRevLetsFunction
    return $ (lshow (explicitParen $ config state) (use_utf8 $ config state)).letsWrapper.wrapper

getRevLetsMap :: State PState (M.Map Term Variable)
getRevLetsMap = do
    state <- get
    let relevant = filter (\(a,b) -> a `elem` reverseLets state) $ binders state
    return (M.fromList $ map (\(a, b) -> (b, a)) $ relevant)

getRevLetsFunction :: State PState (Term -> Term)
getRevLetsFunction = do
    map <- getRevLetsMap
    return $ applyRevLets' map [] 


applyRevLets :: Term -> State PState Term
applyRevLets term = do
    state <- get
    let relevant = filter (\(a,b) -> a `elem` reverseLets state) $ binders state
    return $ applyRevLets' (M.fromList $ map (\(a, b) -> (b, a)) $ relevant) [] term 


applyRevLets' :: M.Map Term Variable -> [Variable] -> Term -> Term
applyRevLets' map l term = case M.lookup term map of
                           Just v -> if v `elem` l then term else V v
                           Nothing -> case term of
                                        V v -> V v
                                        A p q -> A (applyRevLets' map l p) (applyRevLets' map l q)
                                        L x p -> L x (applyRevLets' map (x:l) p) 
