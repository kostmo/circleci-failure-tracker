{-# OPTIONS_GHC -Wall -O2 -threaded -with-rtsopts="-N" #-}

-- | Found here: https://gist.github.com/Rydgel/938f8479f433accc9ca0

import Control.Concurrent
import Control.Concurrent.Async
import Control.Lens
import Control.Monad
import Data.ByteString.Lazy hiding (replicate)
import Data.List.Split
import Network.Wreq hiding (getWith)
import Network.Wreq.Session

data ThreadOptions = ThreadOptions { wreqOptions :: Options, interval :: Int }

data GlobalOptions = GlobalOptions { threadOptions :: ThreadOptions, threadCount :: Int }

defaultWreqOptions :: Options
defaultWreqOptions = defaults

defaultThreadOptions :: ThreadOptions
defaultThreadOptions = ThreadOptions { wreqOptions = defaultWreqOptions, interval = 0 }

defaultGlobalOptions :: GlobalOptions
defaultGlobalOptions = GlobalOptions { threadOptions = defaultThreadOptions, threadCount = 8 }

thread :: ThreadOptions -> [String] -> IO [ByteString]
thread options urls = withSession $ \session -> 
    mapM (\url -> const . flip (^.) responseBody <$>
       getWith (wreqOptions options) session url
    <*> threadDelay (interval options)) urls

threads :: GlobalOptions -> [String] -> IO [ByteString]
threads options urls = join <$> mapConcurrently (thread (threadOptions options)) (chunksOf (threadCount options) urls)

main :: IO ()
main = do
    contents <- threads defaultGlobalOptions (replicate 8 "https://httpbin.org/")
    print contents
