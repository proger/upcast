{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ImplicitParams #-}

module Upcast.Environment where

import           Control.Applicative
import           System.Directory (canonicalizePath)
import           System.Posix.Env (getEnv)

import qualified Data.ByteString.Char8 as B8
import           Data.Maybe (isJust)

import           Upcast.Deploy (nixCopyClosureTo, nixRealise, nixSetProfile)
import           Upcast.IO (srsly)
import           Upcast.Monad (sequenceMaybe, when)
import           Upcast.Shell
import           Upcast.Types (NixContext(..), Build(..))

nixContext :: FilePath -> [String] -> IO NixContext
nixContext file args = do
    nix_expressionFile <- canonicalizePath file
    let nix_args = ["--show-trace"] <> args
    return NixContext{..}

nixInstantiate :: [String] -> Maybe String -> String -> FilePath -> Commandline
nixInstantiate nix_args attr exprFile root =
  exec "nix-instantiate" (nix_args <>
                          [ "--add-root", root
                          , "--indirect"
                          ] <> maybeKey "-A" attr <> [exprFile])


build :: Build -> IO FilePath
build Build{..} = nixContext b_expressionFile b_extra >>= go
  where
    ssh_ = case b_builder of
                "localhost" -> id
                _ -> ssh b_builder []
    fwd = fgrunDirect . ssh_
    copy = let ?sshConfig = Nothing in nixCopyClosureTo

    go :: NixContext -> IO FilePath
    go NixContext{..} = do
      drv <- fgtmp (nixInstantiate nix_args b_attribute nix_expressionFile)

      srsly "nix-copy-closure failed" (fgrunDirect (copy b_builder drv))
      srsly "realise failed" (fwd (nixRealise drv))
      out <- B8.unpack <$> fgconsume_ (ssh_ (exec "nix-store" ["-qu", drv]))
      when b_cat $ do
        fwd (exec "cat" [toString out])
        return ()
      when (isJust b_installProfile) $ do
        let Just prof = b_installProfile
        fwd (nixSetProfile prof out)
        return ()
      return out
