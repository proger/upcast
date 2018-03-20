module Upcast.Types where

import Data.Text (Text)
import Data.ByteString.Char8 (ByteString)

newtype Remote = Remote String
                 deriving Show

type StorePath = FilePath
type StorePathBS = ByteString
type Hostname = Text
type Executable = String

data NixContext =
  NixContext
  { nix_expressionFile :: FilePath
  , nix_args :: [String]
  } deriving (Show)

-- | Per-machine Nix closure install context used during 'upcast install'.
data Install =
  Install
  { i_remote :: Remote
  , i_profile :: FilePath
  , i_sshConfig :: Maybe FilePath
  , i_delivery :: DeliveryMode
  , i_storepath :: StorePath
  } deriving (Show)

data DeliveryMode = Push | Pull String
                  deriving Show

-- | Arguments to 'build'.
data Build =
  Build
  { b_builder :: String
  , b_attribute :: Maybe String
  , b_cat :: Bool
  , b_installProfile :: Maybe FilePath
  , b_expressionFile :: FilePath
  , b_extra :: [String]
  } deriving (Show)
