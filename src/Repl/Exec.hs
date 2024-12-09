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
import System.IO (hGetContents)

type Repl = HaskelineT (Exec IO)

-- discard error and continue to run repl 
continueBy :: (MonadIO m, MonadCatch m) => m () -> m ()
continueBy m = m `catch` \(SomeException e) -> print e

exec :: String -> Repl ()
exec source = continueBy $ do
  stmt <- doParse parseStmt "<stdin>" (pack source)
  lift $ evalStmt stmt

-------------------------------------------------------------------------------
-- Command Options
-------------------------------------------------------------------------------

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
loadFile :: String -> Repl ()
loadFile fName = continueBy $ do
  stmts <- liftIO $ withFile fName ReadMode $ \hdl -> do
    contents <- hGetContents hdl
    doParseFile parseFileContents fName (pack contents)
  lift $ mapM_ evalStmt stmts

options :: Options Repl
options = [
    ("q"   , quit)
  , ("t", seeType)
  , ("l", loadFile)
  ]

-------------------------------------------------------------------------------
-- REPL details
-------------------------------------------------------------------------------

defaultMatcher :: [(String, CompletionFunc m)]
defaultMatcher = []

-- Default tab completer
comp :: Monad m => WordCompleter m
comp n = do
  let cmds = [":q", ":t", ":l"]
  return $ filter (isPrefixOf n) cmds

completer :: CompleterStyle (Exec IO)
completer = Prefix (wordCompleter comp) defaultMatcher

ini :: Repl ()
ini = do
  loadFile "core/bool.sadt"
  loadFile "core/nat.sadt"
  putStrLn "REPL for simpleADT"

final :: Repl ExitDecision
final = do
  liftIO $ putStrLn "Goodbye!"
  return Exit

-------------------------------------------------------------------------------
-- Entry Point
-------------------------------------------------------------------------------

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
