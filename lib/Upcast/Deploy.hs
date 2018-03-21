{-# LANGUAGE ViewPatterns #-}

module Upcast.Deploy where

import           System.Directory (canonicalizePath)
import           System.Environment (lookupEnv)
import           Data.List (break)
import qualified Data.ByteString.Char8 as B8

import           Upcast.IO
import           Upcast.Monad
import qualified Upcast.Shell as Shell
import           Upcast.Shell hiding (ssh, fgrun)
import           Upcast.Types
import           Upcast.SSH

fgrun :: Commandline -> IO ()
fgrun = expect ExitSuccess "install step failed" . Shell.fgrun

nixRealise :: StorePath -> Commandline
nixRealise (StorePath drv) = exec "nix-store" ["-Q", "--realise", drv]

nixSetProfile :: NixProfile -> StorePath -> Commandline
nixSetProfile (NixProfile i_profile) (StorePath i_storepath) =
  sudo (exec "nix-env" ["-p", i_profile , "--set", i_storepath])

nixCopyClosureTo :: (?sshConfig :: SshConfig) => Remote -> StorePath -> Commandline
nixCopyClosureTo remote (StorePath path) =
  case remote of
    Remote "localhost" -> exec "ls" ["-ld", "--", path]
    Remote host -> nixSshEnv (exec "nix-copy-closure" [ "--gzip", "--sign", "--to", host, path ])

nixCopyClosureToI :: (?sshConfig :: SshConfig) => Install -> Commandline
nixCopyClosureToI Install{i_target, i_storepath} =
  nixCopyClosureTo i_target i_storepath

nixCopyClosureFromI :: (?sshConfig :: SshConfig) => Remote -> Install -> Commandline
nixCopyClosureFromI (Remote from) Install{i_target, i_storepath} =
  ssh i_target (nixSshEnv (exec "nix-copy-closure" ["--gzip", "--sign", "--from", from
                                                   , unStorePath i_storepath]))

nixSystemProfile :: NixProfile
nixSystemProfile = "/nix/var/nix/profiles/system"

nixSetProfileI :: (?sshConfig :: SshConfig) => Install -> Commandline
nixSetProfileI Install{i_target, i_profile, i_storepath} =
  ssh i_target (nixSetProfile i_profile i_storepath)

nixSwitchToConfiguration :: (?sshConfig :: SshConfig) => Install -> Commandline
nixSwitchToConfiguration Install{i_target, i_storepath} =
  ssh i_target (sudo (env [("NIXOS_NO_SYNC", "1")]
                      (exec (unStorePath i_storepath <> "/bin/switch-to-configuration") ["switch"])))

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
prepKnownHostI cache Install{i_target} = ssh i_target (prepKnownHost cache)

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

copyKeys :: (?sshConfig :: SshConfig) => Remote -> Commandline
copyKeys remote =
  copyAsRoot remote "/etc/nix/signing-key.pub"
  <> copyAsRoot remote "/etc/nix/signing-key.sec"

install :: Install -> IO ()
install install@Install{..} = do
  let ?sshConfig = i_sshConfig

  mcache <- lookupEnv "UPCAST_SSH_STORE_CACHE"
  case mcache of
    Nothing    -> return ()
    Just cache -> fgrun (prepKnownHostI cache install)

  fgrun (copyKeys i_target)

  case i_delivery of
    Push                      -> fgrun (nixCopyClosureToI install)
    Pull (Remote "localhost") -> return ()
    Pull from                 -> fgrun (nixCopyClosureFromI from install)

  fgrun (nixSetProfileI install)
  when i_switch (fgrun (nixSwitchToConfiguration install))


nixQueryDrvOutput :: StorePath -> Commandline
nixQueryDrvOutput (StorePath drv) = exec "nix-store" ["-qu", drv]

canonicalizeNixPath :: FilePath -> IO FilePath
canonicalizeNixPath path@('<':_) = return path
canonicalizeNixPath path = canonicalizePath path

mkdir :: FilePath -> Commandline
mkdir dir = exec "mkdir" ["-p", dir]

testFile :: FilePath -> Commandline
testFile fp = exec "test" ["-f", fp]

nqPath, fqPath :: FilePath
nqPath = "/nix/var/nix/profiles/nq/bin/nq"
fqPath = "/nix/var/nix/profiles/nq/bin/fq"

nq :: Nqdir -> Commandline -> Commandline
nq (Nqdir nqdir) cmd = mkdir nqdir <> env [("NQDIR", nqdir)] (exec nqPath ["sh", "-c", sh cmd])

fq :: Nqdir -> Commandline
fq (Nqdir nqdir) = env [("NQDIR", nqdir)] (exec fqPath [])

build :: Build -> IO StorePath
build Build{..} = do
  let ?sshConfig = b_sshConfig
  let ssh_ = ssh b_target
  let instantiate =
        case b_buildMode of
          BuildPackage -> nixInstantiate
          BuildNixos -> nixInstantiateNixos
  let needsNq = maybe False (const True) b_nqdir

  when needsNq (fgrun (ssh_ (testFile nqPath)))

  nix_expressionFile <- canonicalizeNixPath b_expressionFile
  let nix_args = ["--show-trace"] <> b_extra
  drv <- fmap StorePath (fgtmp (instantiate nix_args b_attribute nix_expressionFile))

  fgrun (copyKeys b_target)
  fgrun (nixCopyClosureTo b_target drv)

  case b_nqdir of
    Nothing -> fgrun (ssh_ (nixRealise drv))
    Just nqdir -> do
      fgrun (ssh_ (nq nqdir (nixRealise drv)))

      warn [applyColor Green ("it is ok to Ctrl-C after nq -- nix-store --realise will run in the background.")]
      case b_installProfile of
        Nothing -> return ()
        Just prof -> do
          warn [applyColor Red ("however if you detach you will have to install "
                                 <> show prof <> " manually later")]


      fgrun (ssh_ (fq nqdir))

  out <- fmap (StorePath . B8.unpack) (fgconsume_ (ssh_ (nixQueryDrvOutput drv)))

  when b_cat (fgrun (ssh_ (cat1 (unStorePath out))))

  case b_installProfile of
    Nothing   -> return ()
    Just prof -> fgrun (ssh_ (nixSetProfile prof out))

  return out
