module CircleApi where

import           Data.Text (Text)
import           Web.JWT   as JWT


newtype CircleCIApiToken = CircleCIApiToken Text


data ThirdPartyAuth = ThirdPartyAuth {
    circle_api_token :: CircleCIApiToken
  , jwt_signer       :: JWT.Signer
  }
