module AuthConfig where

import           Data.Text      (Text)


data GithubConfig = GithubConfig {
    is_local :: Bool
  , client_secret :: Text
  }

