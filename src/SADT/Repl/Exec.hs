module SADT.Repl.Exec (runRepl) where

import SADT.Common
import SADT.Parser.Expr
import SADT.Parser.Stmt
import SADT.Repl.State
import SADT.Repl.Stmt
import SADT.Typing.Infer
import SADT.Util.Directory
import System.Console.Repline hiding (options)

type Repl = HaskelineT (Exec IO)

-- discard error and continue to run repl
continueBy :: (MonadIO m, MonadCatch m) => m () -> m ()
continueBy m = m `catch` \(SomeException e) -> print e

exec :: String -> Repl ()
exec source = continueBy $ do
  stmt <- parseStmt "<stdin>" source
  lift $ evalStmt stmt

evalFileContent ::
  (MonadTrans t, MonadIO m, MonadIO (t (Exec m)), MonadCatch m) =>
  FilePath ->
  t (Exec m) ()
evalFileContent fp = do
  stmts <- liftIO $ getFileContents fp
  lift $ mapM_ evalStmt stmts

-------------------------------------------------------------------------------
-- Command Options
-------------------------------------------------------------------------------

-- :q
quit :: String -> Repl ()
quit _ = exitSuccess

-- :t
seeType :: String -> Repl ()
seeType source = continueBy $ do
  (te, ce) <- getTyAndConsEnv
  e <- parseExpr "<stdin>" source
  t <- runInfer e te ce []
  liftIO $ print t

-- :l
loadFile :: String -> Repl ()
loadFile fName = continueBy $ evalFileContent fName

options :: Options Repl
options =
  [ ("q", quit),
    ("t", seeType),
    ("l", loadFile)
  ]

-------------------------------------------------------------------------------
-- REPL details
-------------------------------------------------------------------------------

defaultMatcher :: [(String, CompletionFunc m)]
defaultMatcher = []

-- Default tab completer
comp :: (Monad m) => WordCompleter m
comp n = do
  let cmds = [":q", ":t", ":l"]
  return $ filter (isPrefixOf n) cmds

completer :: CompleterStyle (Exec IO)
completer = Prefix (wordCompleter comp) defaultMatcher

ini :: Repl ()
ini = do
  fps <- liftIO $ getAllFilePaths "core"
  forM_ fps evalFileContent

final :: Repl ExitDecision
final = do
  liftIO $ putStrLn "Goodbye!"
  return Exit

-------------------------------------------------------------------------------
-- Entry Point
-------------------------------------------------------------------------------

runRepl :: IO ()
runRepl =
  runExec $
    evalRepl
      (const . pure $ "repl> ")
      exec
      options
      (Just ':')
      (Just "paste")
      completer
      ini
      final
