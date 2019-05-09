module Main where

import Network.Simple.TCP.TLS
import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack, unpack)
import System.Environment

-- Usage: cutie "example.irc.network" 6697 cutie_bot

main :: IO ()
main = do (address:port:nick:_) <- getArgs -- gives an awful error, TODO: fix
          -- Only one connection at once, so having the ServiceID suffix be empty is fine
          clientSettings <- getDefaultClientSettings (address, B.empty)
          connect clientSettings address port
            $ \(context, remoteAddr) ->
                do putStrLn $ "Connection established to " ++ show remoteAddr
                   send context $ pack $ "NICK " ++ nick ++ "\r\n"
                   send context $ pack $ "USER " ++ nick ++" 0 * :Cutie bot!\r\n"
                   sockLoop context
                     where
                       sockLoop c =
                         do Just dat <- recv c
                            putStr $ unpack dat
                            sockLoop c
  
