module Main where

import           Options.Applicative
import           Data.Monoid
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import           Upcast.Deploy
import           Upcast.IO
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

    opts = subparser cmds `info`
           (header "upcast - nix orchestration"
            <> footerDoc (Just (PP.string examples)))

    cmds = command "build"
           ((putStrLn . unStorePath <=< build) <$> buildCli `info`
            progDesc "nix-instantiate locally, nix-store --realise remotely")

        <> command "install"
           (install <$> installCli `info`
            progDesc ("smart nix-copy-closure and nix-env --profile --set"))

    exp = metavar "<expression file>"

    nixArgs = many (argument str (metavar "nix arguments..."))

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
      mkNixProfile <$> strOption (long "profile"
                                  <> short 'p'
                                  <> metavar "PROFILE"
                                  <> help help')

    deliveryMode =
      (Pull . Remote <$>
       strOption (long "pull"
                  <> short 'f'
                   <> metavar "FROM"
                   <> help "try to pull store paths from host (relative to ADDRESS)"))
      <|> pure Push

    attribute =
      AttrName <$> strOption (short 'A'
                               <> metavar "ATTRIBUTE"
                               <> help "build a specific attribute in the expression file")

    storePath =
      StorePath <$> argument str (metavar "STORE_PATH")

    nqDir =
      Nqdir <$> strOption (long "nqdir"
                           <> short 'N'
                           <> metavar "NQDIR"
                           <> help ("enqueue the build job using nq(1), needs "
                                     <> nqPath <> " to be available."))

    installCli = Install
      <$> remoteHost
      <*> (profile ("set STORE_PATH to PROFILE (defaults to " <> show nixSystemProfile <> "). prepends /nix/var/nix/profiles if needed.")
           <|> pure nixSystemProfile)
      <*> ssh_config
      <*> deliveryMode
      <*> switch (long "switch"
                  <> short 's'
                  <> help "run `STORE_PATH/bin/switch-to-configuration switch' after copy")
      <*> storePath

    buildCli = Build
      <$> remoteHost
      <*> ssh_config
      <*> switch (long "cat"
                  <> short 'c'
                  <> help "try to cat output store path contents to stdout after build")
      <*> optional nqDir
      <*> optional (profile "set the output store path to PROFILE on the HOST. prepends /nix/var/nix/profiles if needed.")
      <*> flag BuildPackage BuildNixos (long "nixos"
                                         <> short 'n'
                                         <> help "the expression to build is nixos configuration")
      <*> optional attribute
      <*> argument str exp
      <*> nixArgs


examples = "Examples:\n\n\
           \  # build pkgs.nq for linux on example.com and install it to /nix/var/nix/profiles/nq\n\
           \  upcast build -t example.com -p nq -A nq '<nixpkgs>' -- --argstr system x86_64-linux\n\
           \\n\
           \"
