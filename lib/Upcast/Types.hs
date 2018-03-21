module Upcast.Types where

import Data.String

newtype Remote = Remote String deriving Show
newtype SshConfig = SshConfig { unSshConfig :: Maybe FilePath } deriving Show
newtype StorePath = StorePath { unStorePath :: FilePath } deriving Show
newtype AttrName = AttrName { unAttrName :: String } deriving Show
newtype NixProfile = NixProfile { unNixProfile :: FilePath } deriving (Monoid, IsString)
newtype Nqdir = Nqdir { unNqdir :: FilePath } deriving Show

mkNixProfile p@('/':_) = NixProfile p
mkNixProfile p = NixProfile ("/nix/var/nix/profiles/" ++ p)

instance Show NixProfile where
  show (NixProfile p) = show p

-- | Per-machine Nix closure install context used during 'upcast install'.
data Install =
  Install
  { i_target :: Remote
  , i_profile :: NixProfile
  , i_sshConfig :: SshConfig
  , i_delivery :: DeliveryMode
  , i_switch :: Bool
  , i_storepath :: StorePath
  } deriving (Show)

data DeliveryMode
  = Push | Pull Remote
    deriving (Show)

-- | Arguments to 'build'.
data Build =
  Build
  { b_target :: Remote
  , b_sshConfig :: SshConfig
  , b_cat :: Bool
  , b_nqdir :: Maybe Nqdir
  , b_installProfile :: Maybe NixProfile
  , b_buildMode :: BuildMode
  , b_attribute :: Maybe AttrName
  , b_expressionFile :: FilePath
  , b_extra :: [String]
  } deriving (Show)

data BuildMode
  = BuildPackage | BuildNixos
    deriving (Show)
