{-# LANGUAGE OverloadedStrings #-}

module IDP where

import qualified IDP.Github          as IGithub
import qualified Keys
import           Session
import           Types
import      qualified     Utils

import qualified AuthConfig


initIdps :: CacheStore -> AuthConfig.GithubConfig -> IO ()
initIdps c auth_config = insertIDPData c $ mkIDPData auth_config


mkIDPData :: AuthConfig.GithubConfig -> IDPData
mkIDPData auth_config = IDPData (Utils.createCodeUri (Keys.githubKey auth_config) [("state", "Github.test-state-123")]) Nothing (idpLabel IGithub.Github)
