module Main where

import Network.Simple.TCP.TLS
import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack, unpack)
import System.Environment
import qualified Cutie.Protocol as IRC
import Cutie.Commands

-- Usage: cutie "example.irc.network" 6697 cutie_bot

main :: IO ()
main = do (address:port:nick:_) <- getArgs -- gives an awful error, TODO: fix
          -- Only one connection at once, so having the ServiceID suffix be empty is fine
          clientSettings <- getDefaultClientSettings (address, B.empty)
          connect clientSettings address port
            $ \(context, remoteAddr) ->
                do putStrLn $ "Connection established to " ++ show remoteAddr
                   send context $ pack $ "NICK " ++ nick ++ "\r\n"
                   send context $ pack $ "USER " ++ nick ++ " 0 * :Cutie bot!\r\n"
                   sockLoop context

-- I don't like the `IO ()` here, feels quite unclear, but we're doing
-- socket IO. Might be nice to have some bit that would be unwrapped
-- farther up to produce the IO? May be unnecessary though, and we're
-- still logging to console for now, so not quite worth it at the
-- moment honestly
sockLoop :: Context -> IO ()
sockLoop c = do Just dat <- recv c
                putStr $ unpack dat -- log to console
                case (handle $ IRC.decode dat) of
                  Just message -> send c $ IRC.encode message
                  Nothing -> return ()
                sockLoop c

-- Receive a message, and maybe respond, will be moved to another
-- module later down the line.
handle :: Maybe IRC.Message -> Maybe IRC.Message
-- This is definitely kinda hard to mentally parse, maybe I should use
-- view patterns here to convert this to a datatype for handleCommand
handle (Just (IRC.Message _ _ (IRC.PrivMsg _ ('!':command)))) =
  case (handleCommand command) of
    Just m  -> Just $ IRC.Message Nothing Nothing m
    Nothing -> Nothing

handle _ = Nothing
  
