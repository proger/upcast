module Upcast.Types where

newtype Remote = Remote String deriving Show
newtype SshConfig = SshConfig { unSshConfig :: Maybe FilePath } deriving Show
newtype StorePath = StorePath { unStorePath :: FilePath } deriving Show
newtype AttrName = AttrName { unAttrName :: String } deriving Show

-- | Per-machine Nix closure install context used during 'upcast install'.
data Install =
  Install
  { i_target :: Remote
  , i_profile :: FilePath
  , i_sshConfig :: SshConfig
  , i_delivery :: DeliveryMode
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
  , b_installProfile :: Maybe FilePath
  , b_buildMode :: BuildMode
  , b_attribute :: Maybe AttrName
  , b_expressionFile :: FilePath
  , b_extra :: [String]
  } deriving (Show)

data BuildMode
  = BuildPackage | BuildNixos
    deriving (Show)
