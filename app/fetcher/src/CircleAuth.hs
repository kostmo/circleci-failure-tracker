{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module CircleAuth where

import           Control.Applicative
import           Control.Lens               hiding (iat, (<.>))
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
import           Data.Aeson
import qualified Data.ByteString            as B
import           Data.Either.Utils          (maybeToEither)
import           Data.List                  (intercalate)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as E
import           Data.Time
import qualified Data.Time.Clock            as Clock
import           GHC.Generics
import qualified Network.OAuth.OAuth2       as OAuth2
import qualified Network.Wreq               as NW
import           Prelude                    hiding (exp)
import           Web.JWT                    as JWT

import qualified Constants
import qualified FetchHelpers


data AppInstallationTokenResponse = AppInstallationTokenResponse {
    token      :: OAuth2.AccessToken
  , expires_at :: UTCTime
  } deriving (Show, Generic)

instance FromJSON AppInstallationTokenResponse


loadRsaKey :: B.ByteString -> Either T.Text JWT.Signer
loadRsaKey pem_content =
  maybeToEither "no key" $ JWT.RSAPrivateKey <$> rsa_secret
  where
    rsa_secret = readRsaSecret pem_content


getGitHubAppInstallationToken ::
     JWT.Signer
  -> IO (Either String AppInstallationTokenResponse)
getGitHubAppInstallationToken signer = do

  now <- liftIO getCurrentTime
  let later = Clock.addUTCTime ten_minutes now

      cs = mempty { -- mempty returns a default JWTClaimsSet
          iss = stringOrURI $ T.pack $ show Constants.gitHubAppId
        , iat = JWT.numericDate $ get_utc_epoch_seconds now
        , exp = JWT.numericDate $ get_utc_epoch_seconds later
        }


      web_token_text = JWT.encodeSigned
        signer
        (mempty {alg = Just JWT.RS256})
        cs

      opts = NW.defaults
        & NW.header "Authorization" .~ ["Bearer " <> E.encodeUtf8 web_token_text]
        & NW.header "Accept" .~ ["application/vnd.github.machine-man-preview+json"]

  runExceptT $ do
    api_response <- ExceptT $ FetchHelpers.safeGetUrl $
      NW.postWith opts installation_token_fetch_url B.empty

    r <- NW.asJSON api_response
    return $ r ^. NW.responseBody

  where
    get_utc_epoch_seconds :: (FormatTime a) => a -> NominalDiffTime
    get_utc_epoch_seconds = fromInteger . read . formatTime defaultTimeLocale "%s"

    ten_minutes = 60 * 10

    installation_token_fetch_url = intercalate "/" [
        "https://api.github.com/app/installations"
      , show Constants.pytorchGitHubAppInstallationId
      , "access_tokens"
      ]

