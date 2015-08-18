{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Upcast.Infra.Match where

import           Prelude hiding (sequence)

import           Control.Applicative
import           Control.Lens hiding ((.=))

import           Control.Monad hiding (sequence)
import           Control.Monad.Catch (MonadCatch, try, throwM)
import           Control.Monad.Error.Class (MonadError, throwError)
import           Control.Monad.Free.Class (MonadFree)
import           Control.Monad.Trans.AWS (AWSRequest, AWSPager, AWST)
import qualified Control.Monad.Trans.AWS as AWS
import           Control.Monad.Trans.Resource

import           Data.Aeson (ToJSON, Value, object, (.=))
import qualified Data.Map as Map
import           Data.Maybe (maybeToList)
import           Data.Monoid
import           Data.Proxy
import           Data.Text (Text)
import           Data.Traversable (sequence, traverse)
import           GHC.Generics

import           Data.Conduit (($$), (=$=))
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL

import           Network.AWS (MonadAWS, Error)
import qualified Network.AWS.EC2 as EC2
import qualified Network.AWS.EC2.Types as EC2
import qualified Network.AWS.ELB as ELB
import qualified Network.AWS.ELB.Types as ELB
import           Network.AWS.Free (Command)
import           Network.AWS.Pager

import           Upcast.Infra.NixTypes
import           Upcast.Infra.Types
import           Upcast.IO (expectRight)
import           Upcast.Infra.AmazonkaTypes (validateRegion)


data DiscoveryError = NotFound
                    | Ambiguous [Text]
                    deriving (Show, Generic)
instance ToJSON DiscoveryError

type ResourceId = Text

type CanMatch infra =
  ( AWSMatchRequest infra
  , AWSExtractIds (AWS.Rs (Rq infra))
  , AWSRequest (Rq infra)
  , AWSPager (Rq infra)
  , AWSExtractResponse infra
  )

class AWSExtractIds a where
  extractIds :: a -> [ResourceId]

class AWSMatchRequest infra where
  type Rq infra
  matchRequest :: infra -> Tags -> Rq infra
  matchIds :: [ResourceId] -> proxy infra -> Rq infra

instance AWSMatchRequest Ec2instance where
  type Rq Ec2instance = EC2.DescribeInstances

  matchRequest _ tags = EC2.describeInstances & EC2.diiFilters .~ alive:(filters tags)
    where
      alive = EC2.filter' "instance-state-name" & EC2.fValues .~ ["pending", "running", "stopped"]
  matchIds ids _ = EC2.describeInstances & EC2.diiInstanceIds .~ ids

instance AWSExtractIds EC2.DescribeInstancesResponse where
  extractIds resp = do
    rs <- resp ^. EC2.dirsReservations
    inst <- rs ^. EC2.rInstances
    return $ inst ^. EC2.insInstanceId


instance AWSMatchRequest Ec2keypair where
  type Rq Ec2keypair = EC2.DescribeKeyPairs
  matchRequest _ tags = EC2.describeKeyPairs & EC2.dkpsFilters .~ filters tags
  matchIds ids _ = EC2.describeKeyPairs & EC2.dkpsFilters .~ filterIds "key-name" ids

instance AWSExtractIds EC2.DescribeKeyPairsResponse where
  extractIds resp = do
    kpi <- resp ^. EC2.dkprsKeyPairs
    maybeToList $ kpi ^. EC2.kpiKeyName


instance AWSMatchRequest Ebs where
  type Rq Ebs = EC2.DescribeVolumes
  matchRequest Ebs{..} tags = EC2.describeVolumes &
                              EC2.desFilters .~ filters (("Name", ebs_name):tags)
  matchIds ids _ = EC2.describeVolumes & EC2.desVolumeIds .~ ids

instance AWSExtractIds EC2.DescribeVolumesResponse where
  extractIds resp = do
    vol <- resp ^. EC2.dvvrsVolumes
    return $ vol ^. EC2.vVolumeId


instance AWSMatchRequest Ec2sg where
  type Rq Ec2sg = EC2.DescribeSecurityGroups
  matchRequest _ tags = EC2.describeSecurityGroups & EC2.dsgsFilters .~ filters tags
  matchIds ids _ = EC2.describeSecurityGroups & EC2.dsgsGroupIds .~ ids

instance AWSExtractIds EC2.DescribeSecurityGroupsResponse where
  extractIds resp = do
    sg <- resp ^. EC2.dsgrsSecurityGroups
    return $ sg ^. EC2.sgGroupId


instance AWSMatchRequest Ec2subnet where
  type Rq Ec2subnet = EC2.DescribeSubnets
  matchRequest _ tags = EC2.describeSubnets & EC2.dsFilters .~ filters tags
  matchIds ids _ = EC2.describeSubnets & EC2.dsSubnetIds .~ ids

instance AWSExtractIds EC2.DescribeSubnetsResponse where
  extractIds resp = do
    subnet <- resp ^. EC2.dsrsSubnets
    return $ subnet ^. EC2.subSubnetId


instance AWSMatchRequest Ec2vpc where
  type Rq Ec2vpc = EC2.DescribeVPCs
  matchRequest _ tags = EC2.describeVPCs & EC2.dvsFilters .~ filters tags
  matchIds ids _ = EC2.describeVPCs & EC2.dvsVPCIds .~ ids

instance AWSExtractIds EC2.DescribeVPCsResponse where
  extractIds resp = do
    vpc <- resp ^. EC2.dvrsVPCs
    return $ vpc ^. EC2.vpcVPCId


instance AWSMatchRequest Elb where
  type Rq Elb = ELB.DescribeLoadBalancers
  matchRequest Elb{..} _ = ELB.describeLoadBalancers & ELB.dlbLoadBalancerNames .~ [elb_name]
  matchIds ids _ = ELB.describeLoadBalancers & ELB.dlbLoadBalancerNames .~ ids

instance AWSExtractIds ELB.DescribeLoadBalancersResponse where
  extractIds resp = do
    elb <- resp ^. ELB.dlbrsLoadBalancerDescriptions
    maybeToList $ elb ^. ELB.lbdLoadBalancerName


instance AWSPager EC2.DescribeVPCs where page _ _ = Nothing
instance AWSPager EC2.DescribeSubnets where page _ _ = Nothing
instance AWSPager EC2.DescribeSecurityGroups where page _ _ = Nothing
instance AWSPager EC2.DescribeKeyPairs where page _ _ = Nothing
instance AWSPager EC2.DescribeVolumes where page _ _ = Nothing


class AWSExtractResponse infra where
  extractResponse :: MonadThrow m => proxy infra -> Either Error [AWS.Rs (Rq infra)]-> m [ResourceId]

  default extractResponse :: (AWSExtractIds (AWS.Rs (Rq infra)),
                              MonadThrow m)
                          => proxy infra
                          -> Either Error [AWS.Rs (Rq infra)]
                          -> m [ResourceId]
  extractResponse _ = either throwM (return . (>>= extractIds))

instance AWSExtractResponse Elb where
  extractResponse _ response = case response of
    Left e -> maybe (throwM e) (return . const []) (e ^? ELB._AccessPointNotFoundException)
    Right r -> return (r >>= extractIds)

instance AWSExtractResponse Ec2sg
instance AWSExtractResponse Ec2subnet
instance AWSExtractResponse Ec2keypair
instance AWSExtractResponse Ec2instance
instance AWSExtractResponse Ec2vpc
instance AWSExtractResponse Ebs

matchTags ::
  forall infra m.
  (CanMatch infra,
   MonadCatch m,
   MonadResource m,
   MonadFree Command m)
  => infra
  -> Tags
  -> m [ResourceId]
matchTags infra tags = try (C.runConduit conduit) >>= extractResponse (Proxy :: Proxy infra)
  where
    conduit = AWS.paginate (matchRequest infra tags) =$= CL.consume

toDiscovery :: [Text] -> Either DiscoveryError Text
toDiscovery [one] = Right one
toDiscovery [] = Left NotFound
toDiscovery many = Left (Ambiguous many)

discover i t = toDiscovery <$> matchTags i t

filters tags = toFilter <$> tags
  where toFilter (k, v) = EC2.filter' (mconcat ["tag:", k]) & EC2.fValues .~ [v]

filterIds tag ids = [EC2.filter' tag & EC2.fValues .~ ids]

matchInfras :: Infras -> IO Value
matchInfras infras@Infras{..} = object <$> do
  env <- AWS.newEnv region AWS.Discover
  runResourceT $ AWS.runAWST env $
    sequence [ ("vpc" .=)      <$> matchWithName infraEc2vpc
             , ("subnet" .=)   <$> match infraEc2subnet
             , ("sg" .=)       <$> match infraEc2sg
             , ("keypair" .=)  <$> match infraEc2keypair
             , ("ebs" .=)      <$> match infraEbs
             , ("instance" .=) <$> matchWithName infraEc2instance
             , ("elb" .=)      <$> match infraElb
             ]
  where
    match, matchWithName ::
      (CanMatch a, Applicative f, MonadCatch f, MonadResource f)
      => Attrs a -> AWST f (Attrs (Either DiscoveryError ResourceId))
    match = traverse (`discover` [realmName])
    matchWithName = Map.traverseWithKey (\k v -> discover v [realmName, ("Name", k)])

    realmName = ("realm", infraRealmName)
    region = validateRegion infras
