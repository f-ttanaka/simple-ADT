module Repl.Exec (runRepl) where

import Common
import Core.Parser (parseExpr)
import Core.Typing.Infer
import           Data.Text         (pack)
import Repl.Parser
import           Repl.Stmt
import Repl.State
import SExpr.Parser
import           System.Console.Repline hiding (options)

type Repl = HaskelineT (Eval IO)

exec :: String -> Repl ()
exec source = do
  stmt <- doParse parseStmt "<stdin>" (pack source)
  lift $ evalStmt stmt

-- :quit command
quit :: String -> Repl ()
quit _ = exitSuccess

seeType :: String -> Repl ()
seeType source = do
  (te,ce) <- getTyAndConsEnv
  e <- doParse parseExpr "<stdin>" (pack source)
  t <- runInfer e te ce
  liftIO $ print t

defaultMatcher :: [(String, CompletionFunc m)]
defaultMatcher = []

-- Default tab completer
comp :: Monad m => WordCompleter m
comp n = do
  let cmds = [":q", ":t"]
  return $ filter (isPrefixOf n) cmds

options :: Options Repl
options = [
    ("q"   , quit)
  , ("t", seeType)
  ]

ini :: Repl ()
ini = putStrLn "REPL for listy"

final :: Repl ExitDecision
final = do
  liftIO $ putStrLn "Goodbye!"
  return Exit

-------------------------------------------------------------------------------
-- Entry Point
-------------------------------------------------------------------------------

completer :: CompleterStyle (Eval IO)
completer = Prefix (wordCompleter comp) defaultMatcher

runRepl :: IO ()
runRepl = runEval $
  evalRepl (const . pure $ "listy> ")
           exec
           options
           (Just ':')
           (Just "paste")
           completer
           ini
           final
