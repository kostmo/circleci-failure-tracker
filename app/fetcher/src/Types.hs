{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeFamilies              #-}

module Types where

import           Control.Concurrent.MVar
import           Data.Aeson
import           Data.Hashable
import qualified Data.HashMap.Strict     as Map
import           Data.Maybe
import           Data.Text.Lazy
import qualified Data.Text.Lazy          as TL
import           GHC.Generics
import           Text.Mustache
import qualified Text.Mustache           as M

type IDPLabel = Text

-- TODO: how to make following type work??
-- type CacheStore = forall a. IDP a => MVar (Map.HashMap a IDPData)
type CacheStore = MVar (Map.HashMap IDPLabel IDPData)

-- * type class for defining a IDP
--
class (Hashable a, Show a) => IDP a

class (IDP a) => HasLabel a where
  idpLabel :: a -> IDPLabel
  idpLabel = TL.pack . show

-- dummy oauth2 request error
--
data Errors = X deriving (Generic, Show)

instance FromJSON Errors where
  parseJSON = genericParseJSON defaultOptions { constructorTagModifier = camelTo2 '_', allNullaryToStringTag = True }

data LoginUser = LoginUser { loginUserName :: Text
            , loginAlias                   :: Text
            } deriving (Eq, Show)

data IDPData =
  IDPData { codeFlowUri     :: Text
          , loginUser       :: Maybe LoginUser
          , idpDisplayLabel :: IDPLabel
          }

-- simplify use case to only allow one idp instance for now.
instance Eq IDPData where
  a == b = idpDisplayLabel a == idpDisplayLabel b

instance Ord IDPData where
  a `compare` b = idpDisplayLabel a `compare` idpDisplayLabel b


-- * Mustache instances
instance ToMustache IDPData where
  toMustache t' = M.object
    [ "codeFlowUri" ~> codeFlowUri t'
    , "isLogin" ~> isJust (loginUser t')
    , "user" ~> loginUser t'
    , "name" ~> TL.unpack (idpDisplayLabel t')
    ]

instance ToMustache LoginUser where
  toMustache t' = M.object
    [ "name" ~> loginUserName t' ]

