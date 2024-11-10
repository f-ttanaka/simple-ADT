module Repl.Exec (runRepl) where

import Common
import           Data.List              (isPrefixOf)
import qualified Data.Map               as M
import           Data.Text         (pack)
import Repl.Parser
import           Repl.Stmt
import Repl.State
import SExpr.Parser
import           System.Console.Repline hiding (options)

type Repl a = Eval (HaskelineT IO) a

-- procStmt (ExprStmt e) = do
--   ist <- get
--   _ <- hoistErr $ runInfer (inferScheme e) (tEnv ist)
--   v <- hoistErr $ runEval (eval e) (vEnv ist)
--   liftIO $ print v
-- procStmt (DefStmt x e) = do
--   ist <- get
--   sc <- hoistErr $ runInfer (inferDef x e) (tEnv ist)
--   -- v <- hoistErr $ runEval (eval e) (vEnv ist)
--   let vEnv' = M.insert x (toClosure vEnv' e) (vEnv ist)
--   put $ ist {vEnv = vEnv', tEnv = M.insert x sc (tEnv ist)}
--   liftIO . putStrLn $ x ++ " : " ++ show sc

exec :: String -> Repl ()
exec source = do
  stmt <- doParse parseStmt "<stdin>" (pack source)
  evalStmt stmt

-- :quit command
quit :: a -> HaskelineT IO ()
quit _ = exitSuccess

-- seeType :: String -> Repl ()
-- seeType source = do
--   ist <- get
--   e <- doParse parseExpr "<stdin>" (pack source)
--   t <- hoistErr $ runInfer (inferScheme e) (tEnv ist)
--   liftIO $ print t

defaultMatcher :: [(String, CompletionFunc m)]
defaultMatcher = []

-- Default tab completer
comp :: Monad m => WordCompleter m
comp n = do
  let cmds = [":q", ":t"]
  return $ filter (isPrefixOf n) cmds

options :: Options (HaskelineT IO)
options = [
    ("q"   , quit)
  --, ("t", seeType)
  ]

ini :: HaskelineT IO ()
ini = putStrLn "REPL for listy"

final :: HaskelineT IO ExitDecision
final = do
  liftIO $ putStrLn "Goodbye!"
  return Exit

-------------------------------------------------------------------------------
-- Entry Point
-------------------------------------------------------------------------------

completer :: CompleterStyle IO
completer = Prefix (wordCompleter comp) defaultMatcher

runRepl :: IO ()
runRepl =
  evalRepl (const . pure $ "listy> ")
           (runEval . exec)
           options
           (Just ':')
           (Just "paste")
           completer ini final
