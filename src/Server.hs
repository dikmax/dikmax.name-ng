module Server (runServer) where

import           BasicPrelude
import           Config
import           System.Process

runServer :: IO ()
runServer = callProcess "firebase" ["emulators:start"]
