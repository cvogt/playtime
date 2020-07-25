module Playtime.LiveCode where

-- This module provides helpers to dynamically compile and load
-- code such as modified game code at runtime.
-- This allows for more interactivity and a quicker feedback loop
-- than restarting the application entirely
--
-- Partially inspired by Bret Victor talk "Inventing on Principle" https://vimeo.com/36579366

-- Blogs used as starting points
-- https://codeutopia.net/blog/2011/08/20/adventures-in-haskell-dynamic-loading-and-compiling-of-modules/
-- https://gist.github.com/jhartikainen/1158986
-- https://bluishcoder.co.nz/2008/11/25/dynamic-compilation-and-loading-of.html

import Bag (bagToList)
import Data.Dynamic
import DynFlags
import qualified EnumSet
import GHC hiding (loadModule)
import GHC.Err (error)
import GHC.LanguageExtensions.Type
import GHC.Paths (libdir)
import HscTypes (SourceError, srcErrorMessages)
import My.IO
import My.Prelude

type String = [Char]

compileAndEvalOrFail :: Typeable a => [FilePath] -> String -> String -> IO a
compileAndEvalOrFail srcFiles modname expr = do
  v <- join $ sequence <$> compileAndEvalMay srcFiles modname expr
  maybe (fail "could coerce return value of dynamically loaded code to expected type") pure v

compileAndEvalMay :: Typeable a => [FilePath] -> String -> String -> IO (Maybe a)
compileAndEvalMay srcFiles modname expr = do
  runGhc (Just libdir) $
    loadSourceGhc srcFiles >>= \case
      Just err -> error err
      Nothing -> evalExpression modname expr

evalExpression :: Typeable a => String -> String -> Ghc (Maybe a)
evalExpression modname expr = do
  mod <- findModule (mkModuleName modname) Nothing
  setContext [IIModule $ moduleName mod]
  fromDynamic <$> dynCompileExpr (modname <> "." <> expr)

loadSourceGhc :: [FilePath] -> Ghc (Maybe String)
loadSourceGhc paths = do
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
    Failed -> pure $ Just $ "Generic module load error"
    Succeeded -> pure Nothing
  `gcatch` \(e :: SourceError) -> pure $ Just $ concat $ fmap show $ bagToList $ srcErrorMessages e
