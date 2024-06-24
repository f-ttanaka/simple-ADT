module Repl.Exec (runRepl) where

import Control.Monad.Check (Check(..))
import           Control.Monad.State    (StateT, evalStateT, get, liftIO, put)
import           Data.List              (isPrefixOf)
import qualified Data.Map               as M
import           Data.Text         (pack)
import Parser.Expr ( parseExpr ) 
import Parser.SExpr (doParse)
import           Repl.Stmt
import Semantics.Eval
import Semantics.Prim
import           System.Console.Repline hiding (options)
import           System.Exit
import           Typing.Infer           (TEnv, runInfer, inferScheme)
import Typing.Prim (initialTEnv)

data IState = IState
  { vEnv :: VEnv
  , tEnv :: TEnv }
  deriving Show

initState :: IState
initState = IState initialVEnv initialTEnv

type Repl a = HaskelineT (StateT IState IO) a

hoistErr :: Check a -> Repl a
hoistErr ch = case runCheck ch of
  Right val -> return val
  Left err -> liftIO (print err) >> abort

procStmt :: Stmt -> Repl ()
procStmt (ExprStmt e) = do
  ist <- get
  _ <- hoistErr $ runInfer (inferScheme e) (tEnv ist)
  v <- hoistErr $ runEval (eval e) (vEnv ist)
  liftIO $ print v
procStmt (DefStmt x e) = do
  ist <- get
  sc <- hoistErr $ runInfer (inferDef x e) (tEnv ist)
  -- v <- hoistErr $ runEval (eval e) (vEnv ist)
  let vEnv' = M.insert x (toClosure vEnv' e) (vEnv ist)
  put $ ist {vEnv = vEnv', tEnv = M.insert x sc (tEnv ist)}
  liftIO . putStrLn $ x ++ " : " ++ show sc

exec :: String -> Repl ()
exec source = do
  stm <- hoistErr $ doParse parseStmt "<stdin>" (pack source)
  procStmt stm

-- :quit command
quit :: a -> Repl ()
quit _ = liftIO exitSuccess

seeType :: String -> Repl ()
seeType source = do
  ist <- get
  e <- hoistErr $ doParse parseExpr "<stdin>" (pack source)
  t <- hoistErr $ runInfer (inferScheme e) (tEnv ist)
  liftIO $ print t

defaultMatcher :: [(String, CompletionFunc m)]
defaultMatcher = []

-- Default tab completer
comp :: Monad m => WordCompleter m
comp n = do
  let cmds = [":q", ":t"]
  return $ filter (isPrefixOf n) cmds

options :: [(String, String -> Repl ())]
options = [
    ("q"   , quit)
  , ("t", seeType)
  ]

ini :: Repl ()
ini = liftIO $ putStrLn "REPL for listy"

final :: Repl ExitDecision
final = do
  liftIO $ putStrLn "Goodbye!"
  return Exit

-------------------------------------------------------------------------------
-- Entry Point
-------------------------------------------------------------------------------

completer :: CompleterStyle (StateT IState IO)
completer = Prefix (wordCompleter comp) defaultMatcher

runRepl :: IO ()
runRepl = flip evalStateT initState
     $ evalRepl (const . pure $ "listy> ") exec options (Just ':') (Just "paste") completer ini final
