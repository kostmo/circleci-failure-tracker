module AuthConfig where

import           Data.Text      (Text)


data GithubConfig = GithubConfig {
    is_local :: Bool
  , client_id :: Text
  , client_secret :: Text
  , personal_access_token :: Text
  }

