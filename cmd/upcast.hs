module Main where

import           Options.Applicative
import           Data.Monoid

import           Upcast.Deploy
import           Upcast.IO
import           Upcast.Install
import           Upcast.Monad
import           Upcast.Types

main :: IO ()
main = do
    hSetBuffering stderr LineBuffering
    join $ customExecParser prefs opts
  where
    prefs = defaultPrefs { prefMultiSuffix = ""
                         , prefDisambiguate = True
                         , prefShowHelpOnError = True
                         , prefShowHelpOnEmpty = True
                         , prefBacktrack = True
                         , prefColumns = 80
                         }

    exp = metavar "<expression file>"
    nixArgs = many (argument str (metavar "nix arguments..."))

    opts = subparser cmds `info` header "upcast - nix orchestration"

    cmds = command "build"
           ((putStrLn . unStorePath <=< build) <$> buildCli `info`
            progDesc "nix-build with remote forwarding")

        <> command "install"
           (install <$> installCli `info`
            progDesc "copy a store path closure and set it to a profile")

    remoteHost =
      Remote <$> (strOption
                   (long "target"
                     <> short 't'
                     <> metavar "HOST"
                     <> help "SSH-accessible host with Nix")
                   <|> pure "localhost")

    ssh_config =
      SshConfig <$> optional (strOption
                              (long "ssh-config"
                               <> short 'c'
                               <> metavar "FILE"
                               <> help "use FILE as ssh_config(5)"))

    profile help' =
      strOption (long "profile"
                  <> short 'p'
                  <> metavar "PROFILE"
                  <> help help')

    deliveryMode =
      ((Pull . Remote) <$>
       strOption (long "pull"
                  <> short 'f'
                   <> metavar "FROM"
                   <> help "try to pull store paths from host (relative to ADDRESS)"))
      <|> pure Push

    installCli = Install
      <$> remoteHost
      <*> (profile ("set STORE_PATH to PROFILE (defaults to " <> nixSystemProfile <> ")")
           <|> pure nixSystemProfile)
      <*> ssh_config
      <*> deliveryMode
      <*> StorePath <$> argument str (metavar "STORE_PATH")

    buildCli = Build
      <$> remoteHost
      <*> ssh_config
      <*> switch (long "cat"
                  <> short 'c'
                  <> help "try to cat output store path contents to stdout after build")
      <*> optional (profile "set the output store path to PROFILE on the HOST")
      <*> flag BuildPackage BuildNixos (long "nixos"
                                         <> short 'n'
                                         <> help "the expression to build is nixos configuration")
      <*> optional (strOption (short 'A'
                     <> metavar "ATTRIBUTE"
                     <> help "build a specific attribute in the expression file"))
      <*> argument str exp
      <*> nixArgs
