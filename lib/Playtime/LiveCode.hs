module Playtime.LiveCode where

-- This module provides helpers to dynamically compile and load
-- code such as modified game code at runtime.
-- This allows for more interactivity and a quicker feedback loop
-- than restarting the application entirely
--
-- Partially inspired by Bret Victor's talk "Inventing on Principle" https://vimeo.com/36579366
--
-- Blogs used as starting points
-- https://codeutopia.net/blog/2011/08/20/adventures-in-haskell-dynamic-loading-and-compiling-of-modules/
-- https://gist.github.com/jhartikainen/1158986
-- https://bluishcoder.co.nz/2008/11/25/dynamic-compilation-and-loading-of.html

import Bag (bagToList)
import Data.Dynamic
import Data.Typeable
import DynFlags
import qualified EnumSet
import GHC hiding (loadModule)
import GHC.LanguageExtensions.Type
import GHC.Paths (libdir)
import HscTypes (SourceError, srcErrorMessages)
import My.IO
import My.Prelude
import System.IO (stderr, stdout)
import System.IO.Silently (hCapture)

type String = [Char]

compileAndEval :: Typeable a => [FilePath] -> String -> String -> IO (Either String a)
compileAndEval srcFiles modname expr = do
  (compileErrors, res) <- hCapture [stdout, stderr] $ runGhc (Just libdir) $ runExceptT $ do
    loadSourceGhc srcFiles
    evalExpression modname expr
  pure $ first (<> compileErrors) res

evalExpression :: forall a. Typeable a => String -> String -> ExceptT String Ghc a
evalExpression modname expr = ExceptT $ do
  mod <- findModule (mkModuleName modname) Nothing
  setContext [IIModule $ moduleName mod]
  maybe (Left $ "could coerce return value of dynamically loaded code to expected type: " <> show (typeOf $ Proxy @a)) Right
    . fromDynamic
    <$> dynCompileExpr (modname <> "." <> expr)

loadSourceGhc :: [FilePath] -> ExceptT String Ghc ()
loadSourceGhc paths = ExceptT $
  do
    dflags <- getSessionDynFlags
    void $ setSessionDynFlags $
      dflags
        { ghcLink = LinkInMemory,
          hscTarget = HscInterpreted,
          -- attempts to improve performance, untested
          optLevel = 0,
          simplPhases = 0,
          debugLevel = 0,
          parMakeCount = Nothing,
          --log_action :: DynFlags -> WarnReason -> Severity -> SrcSpan -> PprStyle -> MsgDoc -> IO (),
          --log_action = \_ _ _ _ _ _ -> putStrLn "ERROR",
          -- we can't see the package.yaml, so we need to specify used extensions here
          extensionFlags =
            foldl
              (flip EnumSet.insert)
              EnumSet.empty
              [ DeriveAnyClass, -- FIXME: probably need to make extensions configurable at some point
                DeriveGeneric,
                LambdaCase,
                PackageImports,
                RecordPuns, -- this is NamedFieldPuns
                RecordWildCards, -- Doesn't seem to work here
                TraditionalRecordSyntax,
                TupleSections,
                ViewPatterns
              ],
          packageFlags = [ExposePackage "ghc" (PackageArg "ghc") $ ModRenaming True []]
        }
    for_ paths $ \path -> addTarget =<< guessTarget path Nothing
    load LoadAllTargets >>= \case
      Failed -> pure $ Left $ "COMPILE ERROR:\n"
      Succeeded -> pure $ Right ()
    `gcatch` \(e :: SourceError) -> pure $ Left $ concat $ fmap show $ bagToList $ srcErrorMessages e
