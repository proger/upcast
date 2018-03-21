{-# LANGUAGE ViewPatterns #-}

module Upcast.Deploy where

import           System.Directory (canonicalizePath)
import           System.Environment (lookupEnv)
import           Data.List (break)
import qualified Data.ByteString.Char8 as B8

import           Upcast.IO (expect)
import           Upcast.Monad
import qualified Upcast.Shell as Shell
import           Upcast.Shell hiding (ssh, fgrun)
import           Upcast.Types
import           Upcast.SSH

fgrun :: Commandline -> IO ()
fgrun = expect ExitSuccess "install step failed" . Shell.fgrun

nixRealise :: StorePath -> Commandline
nixRealise (StorePath drv) = exec "nix-store" ["--realise", drv]

nixSetProfile :: FilePath -> StorePath -> Commandline
nixSetProfile i_profile i_storepath = exec "nix-env" ["-p", i_profile
                                                     , "--set", unStorePath i_storepath]

nixCopyClosureTo :: (?sshConfig :: SshConfig) => Remote -> StorePath -> Commandline
nixCopyClosureTo remote (StorePath path) =
  case remote of
    Remote "localhost" -> exec "ls" ["-ld", "--", path]
    Remote host -> nixSshEnv (exec "nix-copy-closure" [ "--gzip", "--to", host, path ])

nixSystemProfile :: FilePath
nixSystemProfile = "/nix/var/nix/profiles/system"

nixSetProfileI :: (?sshConfig :: SshConfig) => Install -> Commandline
nixSetProfileI Install{i_remote, i_profile, i_storepath} =
  ssh i_remote (nixSetProfile i_profile i_storepath)

nixSwitchToConfiguration :: (?sshConfig :: SshConfig) => Install -> Commandline
nixSwitchToConfiguration Install{i_remote} =
  ssh i_remote (env [("NIXOS_NO_SYNC", "1")]
                    (exec (nixSystemProfile <> "/bin/switch-to-configuration") ["switch"]))

nixCopyClosureToI :: (?sshConfig :: SshConfig) => Install -> Commandline
nixCopyClosureToI Install{i_remote, i_storepath} =
  nixCopyClosureTo i_remote i_storepath

nixCopyClosureFromI :: (?sshConfig :: SshConfig) => Remote -> Install -> Commandline
nixCopyClosureFromI (Remote from) Install{i_remote, i_storepath} =
  ssh i_remote (nixSshEnv (exec "nix-copy-closure" ["--gzip", "--from", from
                                                   , unStorePath i_storepath]))

testClosureCache :: String -> String
testClosureCache cache =
  case break (=='@') cache of
    (_:_, '@':_) -> cache
    _ -> error "ssh closure cache target must look like `user@hostname'"

prepKnownHost :: String -> Commandline
prepKnownHost (testClosureCache -> knownHost) =
  exec "install" ["-m", "700", "-d", "~/.ssh"] <>
  exec "ssh-keygen" ["-R", knownHost] <>
  exec "ssh-keyscan" ["-t", "rsa,dsa", knownHost] |> "~/.ssh/known_hosts"

prepKnownHostI :: (?sshConfig :: SshConfig) => String -> Install -> Commandline
prepKnownHostI cache Install{i_remote} = ssh i_remote (prepKnownHost cache)

copyAsRoot :: (?sshConfig :: SshConfig) => Remote -> FilePath -> Commandline
copyAsRoot remote f =
  cat1 f |: ssh remote (sudo (cat0 |> f))

nixInstantiate :: [String] -> Maybe AttrName -> FilePath -> FilePath -> Commandline
nixInstantiate nix_args attr exprFile root =
  exec "nix-instantiate"
  (nix_args
    <> [ "--add-root", root
       , "--indirect"
       ]
    <> maybeKey "-A" (fmap unAttrName attr)
    <> [exprFile])

nixInstantiateNixos :: [String] -> Maybe AttrName -> FilePath -> FilePath -> Commandline
nixInstantiateNixos nix_args attr exprFile root =
   exec "nix-instantiate"
   (nix_args
     <> [ "-E", "{ file, attr ? null }: "
                <> "(import <nixpkgs/nixos> {"
                <> " configuration ="
                <> " if attr != null then (import file).\"${attr}\" else (import file); "
                <> " system = \"x86_64-linux\"; "
                <> "}).system"
        , "--arg", "file", exprFile
        ]
     <> maybeKey' ["--arg", "attr"] (fmap unAttrName attr)
     <> [ "--add-root", root
        , "--indirect"
        ])

install :: Install -> IO ()
install install@Install{..} = do
  let ?sshConfig = i_sshConfig

  mcache <- lookupEnv "UPCAST_SSH_STORE_CACHE"
  case mcache of
    Nothing    -> return ()
    Just cache -> fgrun (prepKnownHostI cache install)

  case i_delivery of
    Push                      -> fgrun (nixCopyClosureToI install)
    Pull (Remote "localhost") -> return ()
    Pull from                 -> fgrun (nixCopyClosureFromI from install)

  fgrun (nixSetProfileI install)
  when (i_profile == nixSystemProfile) $ fgrun (nixSwitchToConfiguration install)


nixQueryDrvOutput :: StorePath -> Commandline
nixQueryDrvOutput (StorePath drv) = exec "nix-store" ["-qu", drv]

build :: Build -> IO StorePath
build Build{..} = do
  let ?sshConfig = b_sshConfig
  let ssh_ = ssh b_builder
  let instantiate =
        case b_buildMode of
          BuildPackage -> nixInstantiate
          BuildNixos -> nixInstantiateNixos

  nix_expressionFile <- canonicalizePath b_expressionFile
  let nix_args = ["--show-trace"] <> b_extra
  drv <- fmap StorePath (fgtmp (instantiate nix_args b_attribute nix_expressionFile))

  fgrun (nixCopyClosureTo b_builder drv)
  fgrun (nixRealise drv)
  out <- fmap (StorePath . B8.unpack) (fgconsume_ (ssh_ (nixQueryDrvOutput drv)))

  when b_cat (void (fgrun (ssh_ (cat1 (unStorePath out)))))

  case b_installProfile of
    Nothing   -> return ()
    Just prof -> void (fgrun (ssh_ (nixSetProfile prof out)))

  return out
