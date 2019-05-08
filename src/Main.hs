module Main where

import Network.Simple.TCP.TLS
import qualified Data.ByteString as B
import System.Environment

-- Usage: cutie "example.irc.network" 6697

main :: IO ()
main = do (address:port:_) <- getArgs -- gives an awful error, TODO: fix
          -- Only one connection at once, so having the ServiceID suffix be empty is fine
          clientSettings <- getDefaultClientSettings (address, B.empty) 
          connect clientSettings address port
            $ \(context, remoteAddr) ->
                do putStrLn $ "Connection established to " ++ show remoteAddr
