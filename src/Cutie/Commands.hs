{-# LANGUAGE ViewPatterns #-}

module Cutie.Commands where

import qualified Cutie.Protocol as IRC
import Data.List (stripPrefix)

-- Eventually, switch to using these instead of matching strings with
-- stripPrefix.
data Command = Ping
             | Info

-- For any kind of complex command I need some data alongside this,
-- such as our nick, the channel, etc. Even for ping I'm just assuming
-- it's in #general which is bad form and needs to be fixed.
handleCommand :: String -> Maybe IRC.Command
-- TODO: replace stripPrefix with something better for matching a
-- command, maybe `matchCommand`?
handleCommand (stripPrefix "ping" -> Just _) = Just $ IRC.PrivMsg "#general" "pong!"
handleCommand (stripPrefix "info" -> Just _) = Just $
  IRC.PrivMsg "#general" "Hi, I'm cutiebot! I'm an IRC bot written in Haskell by pea/transitracer. You can find my source code at https://github.com/transitracer/cutie"
handleCommand _ = Nothing
