module Playtime.LiveCode where

-- This module provides helpers to dynamically compile and load
-- code such as modified game code at runtime.
-- This allows for more interactivity and a quicker feedback loop
-- than restarting the application entirely
--
-- Partially inspired by Bret Victor's talk "Inventing on Principle" https://vimeo.com/36579366
--
-- Based on code from these blog posts
-- https://codeutopia.net/blog/2011/08/20/adventures-in-haskell-dynamic-loading-and-compiling-of-modules/
-- https://gist.github.com/jhartikainen/1158986
-- https://bluishcoder.co.nz/2008/11/25/dynamic-compilation-and-loading-of.html

import Bag (bagToList)
import Control.Concurrent.MVar (newEmptyMVar, putMVar)
import Control.Concurrent.MVar (swapMVar)
import Data.Aeson (FromJSON, Result (..), ToJSON, Value (Null), fromJSON, toJSON)
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
import Playtime.Types (EngineConfig)
import System.FSNotify hiding (Event)
import System.IO (stderr, stdout)
import System.IO.Silently (hCapture)

type String = [Char]

startLiveCode :: FromJSON a => LiveCodeState -> IO (Maybe a)
startLiveCode lcs = do
  watch lcs
  recoverLiveCodeGameState lcs

liveCodeSwitch :: ToJSON gameState => LiveCodeState -> gameState -> IO ()
liveCodeSwitch lcs@LiveCodeState {..} gameState = do
  readMVar lcsChangeDetected >>= \case
    False -> pure ()
    True ->
      modifyMVar_ lcsCompileError $ \old_err -> do
        void $ swapMVar lcsChangeDetected False
        Playtime.LiveCode.compileAndEval lcsSrcFiles lcsModule lcsExpression >>= \case
          Left compileErrors -> pure $ Just compileErrors
          Right makeEngineConfig' -> do
            swapMVar lcsGameState $ toJSON gameState
            void $ swapMVar lcsEngineConfig =<< makeEngineConfig' lcs
            pure Nothing -- doesn't clear compile errors because EngineConfig has already been replaced

makeLiveCodeState :: (LiveCodeState -> IO EngineConfig) -> [Char] -> [Char] -> FilePath -> [FilePath] -> IO LiveCodeState
makeLiveCodeState makeEngineConfig' lcsModule lcsExpression lcsWatchDir lcsSrcFiles = do
  lcsChangeDetected <- newMVar False
  lcsCompileError <- newMVar Nothing
  lcsGameState <- newMVar Null
  lcsEngineConfig <- newEmptyMVar
  let lcs = LiveCodeState {..}
  putMVar lcsEngineConfig =<< makeEngineConfig' lcs
  pure lcs

data LiveCodeState = LiveCodeState
  { lcsWatchDir :: FilePath,
    lcsSrcFiles :: [FilePath],
    lcsModule :: [Char],
    lcsExpression :: [Char],
    lcsChangeDetected :: MVar Bool,
    lcsCompileError :: MVar (Maybe [Char]),
    lcsEngineConfig :: MVar EngineConfig,
    lcsGameState :: MVar Value
  }

recoverLiveCodeGameState :: FromJSON a => LiveCodeState -> IO (Maybe a)
recoverLiveCodeGameState LiveCodeState {lcsGameState} =
  (fromJSON <$> readMVar lcsGameState) <&> \case
    Error _ -> Nothing
    Success gs -> Just gs

watch :: LiveCodeState -> IO ()
watch LiveCodeState {..} = do
  void $ forkIO $ do
    mgr <- startManagerConf $ WatchConfig DebounceDefault (100 * 1000) True
    -- start a watching job (in the background)
    void $ watchDir mgr lcsWatchDir (\_ -> True) $
      \_ -> void $ swapMVar lcsChangeDetected True
    forever $ threadDelay $ 1000 * 1000

compileAndEval :: Typeable a => [FilePath] -> String -> String -> IO (Either String a)
compileAndEval srcFiles modname expr = do
  putStrLn $ "EVALING " <> modname <> "." <> expr <> " in:"
  for_ srcFiles putStrLn
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
              [ TraditionalRecordSyntax, -- FIXME: probably need to make extensions configurable at some point
                DeriveAnyClass,
                DeriveFunctor,
                DeriveGeneric,
                DerivingStrategies,
                FlexibleContexts,
                FlexibleInstances,
                GeneralizedNewtypeDeriving,
                LambdaCase,
                MultiParamTypeClasses,
                RecordPuns, -- this is NamedFieldPuns
                -- ImplicitPrelude,
                OverloadedStrings,
                PackageImports,
                RecordWildCards,
                ScopedTypeVariables,
                StandaloneDeriving,
                TupleSections,
                TypeApplications,
                TypeOperators,
                ViewPatterns
              ],
          packageFlags = [ExposePackage "ghc" (PackageArg "ghc") $ ModRenaming True []]
        }
    for_ paths $ \path -> addTarget =<< guessTarget path Nothing
    load LoadAllTargets >>= \case
      Failed -> pure $ Left $ "COMPILE ERROR:\n"
      Succeeded -> pure $ Right ()
    `gcatch` \(e :: SourceError) -> pure $ Left $ concat $ fmap show $ bagToList $ srcErrorMessages e