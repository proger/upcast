module Main where

import           Options.Applicative
import           Data.Monoid

import           Upcast.Deploy
import           Upcast.Environment
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
           ((putStrLn <=< build) <$> buildCli `info`
            progDesc "nix-build with remote forwarding")

        <> command "install"
           (install <$> installCli `info`
            progDesc "copy a store path closure and set it to a profile")

    installCli = Install
      <$> (Remote <$> (strOption (long "target"
                                 <> short 't'
                                 <> metavar "ADDRESS"
                                 <> help "SSH-accessible host with Nix")
                       <|> pure "localhost"))
      <*> (strOption (long "profile"
                      <> short 'p'
                      <> metavar "PROFILE"
                      <> help "set STORE_PATH to PROFILE (otherwise system)")
           <|> pure nixSystemProfile)
      <*> optional (strOption
                    (long "ssh-config"
                     <> short 'c'
                     <> metavar "FILE"
                     <> help "use FILE as ssh_config(5)"))
      <*> (Pull <$> strOption (long "pull"
                               <> short 'f'
                               <> metavar "FROM"
                               <> help "pull store paths from host (relative to ADDRESS)")
           <|> pure Push)
      <*> argument str (metavar "STORE_PATH")

    buildCli = Build
      <$> (strOption (long "target"
                    <> short 't'
                    <> metavar "ADDRESS"
                    <> help "SSH-accessible host with Nix") <|> pure "localhost")
      <*> optional (strOption (short 'A'
                     <> metavar "ATTRIBUTE"
                     <> help "build a specific attribute in the expression file"))
      <*> switch (long "print"
                  <> short 'p'
                  <> help "cat the derivation output file after build")
      <*> optional (strOption (short 'i'
                     <> metavar "PROFILE"
                     <> help "set the output store path to PROFILE on the target"))
      <*> argument str exp
      <*> nixArgs
