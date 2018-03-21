{-# LANGUAGE ImplicitParams #-}

module Upcast.SSH where

import qualified Upcast.Shell as Shell
import           Upcast.Shell hiding (ssh)
import           Upcast.Types

ssh :: (?sshConfig :: SshConfig) => Remote -> Commandline -> Commandline
ssh (Remote host) = Shell.ssh host sshBaseOptions

nixSshEnv :: (?sshConfig :: SshConfig) => Commandline -> Commandline
nixSshEnv = env [("NIX_SSHOPTS", render (args sshBaseOptions))]

sshBaseOptions :: (?sshConfig :: SshConfig) => [String]
sshBaseOptions = [ "-A"
                 , "-o", "StrictHostKeyChecking=no"
                 , "-o", "UserKnownHostsFile=/dev/null"
                 , "-o", "PasswordAuthentication=no"
                 , "-o", "PreferredAuthentications=publickey"
                 , "-x"
                 ] <> maybeKey "-F" (unSshConfig ?sshConfig)

copyAsRoot :: (?sshConfig :: SshConfig) => Remote -> FilePath -> Commandline
copyAsRoot remote f =
  cat1 f |: ssh remote (sudo (cat0 |> f))
