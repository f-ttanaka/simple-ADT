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

type Repl = HaskelineT (Exec IO)

-- discard error and continue to run repl 
continueBy :: (MonadIO m, MonadCatch m) => m () -> m ()
continueBy m = m `catch` \(SomeException e) -> print e

exec :: String -> Repl ()
exec source = continueBy $ do
  stmt <- doParse parseStmt "<stdin>" (pack source)
  lift $ evalStmt stmt

-- command options
-- :q
quit :: String -> Repl ()
quit _ = exitSuccess

-- :t
seeType :: String -> Repl ()
seeType source = continueBy $ do
  (te,ce) <- getTyAndConsEnv
  e <- doParse parseExpr "<stdin>" (pack source)
  t <- runInfer e te ce []
  liftIO $ print t

-- :l
-- loadFile :: String -> Repl ()
-- loadFile fName = continueBy $ do

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

completer :: CompleterStyle (Exec IO)
completer = Prefix (wordCompleter comp) defaultMatcher

runRepl :: IO ()
runRepl = runExec $
  evalRepl (const . pure $ "repl> ")
           exec
           options
           (Just ':')
           (Just "paste")
           completer
           ini
           final
