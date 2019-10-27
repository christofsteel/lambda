module Lambda.REPL
  ( runRepl
  )
where

import           Data.List
import qualified Text.Read                     as TR
import qualified Data.Map as M

import           System.IO
import           System.Exit
import           Data.Maybe
import           Data.Char
import           Control.Monad.State.Strict
import           System.FilePath.Posix
import           System.Console.Haskeline
import           System.Console.Haskeline.Completion
import           Control.Exception             as E

import           Lambda.Types
import           Lambda.Program
import           Lambda.ParserHelper

completableWords =
  [ "let"
  , "print"
  , "printLn"
  , "printT"
  , "traceNF"
  , "traceNFMax"
  , "printNF"
  , "import"
  , "set"
  , "get"
  , "addRev"
  , "delRev"
  , "showRev"
  ]

isCIPrefixOf n m = map toUpper n `isPrefixOf` map toUpper m

runRepl :: FilePath -> Bool -> Bool -> IO ()
runRepl importPath u8 ex = evalStateT
  (runInputT lambdaSettings runStateRepl)
  (defaultState { binders       = []
                , importPath    = importPath
                , config = updateConfig "utf8" u8 $ updateConfig "explicit" ex defaultConfig
                }
  )

searchFunction :: String -> String -> StateT PState IO [Completion]
searchFunction revfirst =
  searchFunction' ((unwords . words . reverse) revfirst)

searchFunction' :: String -> String -> StateT PState IO [Completion]
searchFunction' "import" start = listFiles start
searchFunction' pre start
  | pre == "get" || pre == "set" = do
      state <- get
      return $ map simpleCompletion $ filter (start `isCIPrefixOf`) $ M.keys (config state)
  | pre == "addRev" = do
      state <- get
      return $ map simpleCompletion $ map fst $ binders state
  | pre == "delRev" = do
      state <- get
      return $ map simpleCompletion $ reverseLets state
  | otherwise = do
      state <- get
      let names = completableWords ++ map fst (binders state)
      return $ map simpleCompletion $ filter (start `isPrefixOf`) names


lambdaComplete :: CompletionFunc (StateT PState IO)
lambdaComplete = completeWordWithPrev Nothing " \t" searchFunction

lambdaSettings :: Settings (StateT PState IO)
lambdaSettings = Settings { historyFile    = Nothing
                          , autoAddHistory = True
                          , complete       = lambdaComplete
                          }

trim = unwords . words

parseCommand :: String -> State PState Command
parseCommand line = do
    state <- get
    case TR.readEither $ trim line of
      Left x -> case TR.readEither $ "printNFMax "++ (show $ steps $ config state) ++ " " ++ trim line of
                  Left  err -> return $ Print $ x ++ " `printNFMax " ++ (show $ steps $ config state) ++ " " ++ trim line ++ "`"
                  Right c   -> return c
      Right c -> return c

runStateRepl :: InputT (StateT PState IO) ()
runStateRepl = do
  state <- lift get
  line  <- getInputLine $ ps1 (config state) ++ "> "
  case line of
    Nothing     -> return ()
    Just "exit" -> return ()
    Just line   -> do
     command <- lift $ liftId $ parseCommand line
     case command of
      (Import f) -> do
        contentEx <- liftIO $ try $ readFile $ importPath state </> f
        case contentEx of
          Left  ex      -> liftIO (print (ex :: IOError)) >> runStateRepl
          Right content -> do
            let prog          = read $ replace '\n' ';' $ init content
            let rememberIPath = importPath state
            lift $ put
              (state { importPath = takeDirectory $ importPath state </> f })
            lift $ runIO prog
            state <- lift get
            lift $ put (state { importPath = rememberIPath })
            runStateRepl
      command -> do
        output <- lift $ liftId $ runStep command
        liftIO $ printMaybe output
        liftIO $ putStrLn ""
        runStateRepl


