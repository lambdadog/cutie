{-# LANGUAGE ViewPatterns #-}

module Cutie.Commands where

import qualified Cutie.Protocol as IRC
import Data.List (stripPrefix)

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Either.Combinators (rightToMaybe)
import Data.Void (Void)
import Data.Char

-- Eventually, switch to using these instead of matching strings with
-- stripPrefix.
data Command = Ping
             | Info
  deriving (Show)

buildCommand :: String -> Maybe Command
buildCommand = rightToMaybe . parse commandParser "bot_command"


-- For any kind of complex command I need some data alongside this,
-- such as our nick, the channel, etc. Even for ping I'm just assuming
-- it's in #general which is bad form and needs to be fixed.
handleCommand :: Maybe Command -> Maybe [IRC.Command]
-- TODO: replace stripPrefix with something better for matching a
-- command, maybe `matchCommand`?
handleCommand (Just Ping) = Just [IRC.PrivMsg "#general" "pong!"]
handleCommand (Just Info) = Just $
  [IRC.PrivMsg "#general" "Hi, I'm cutiebot! I'm an IRC bot written in Haskell by pea/transitracer. You can find my source code at https://github.com/transitracer/cutie"]
handleCommand _ = Nothing

-- Parser Code

type Parser = Parsec Void String

-- Copied over from Protocol.hs. TODO: move parsing stuff into
-- Parser.hs to avoid code duplication
notSpaceChar = satisfy $ (not . isSpace)

commandParser :: Parser Command
commandParser = do name <- some notSpaceChar
                   _ <- optional $ some spaceChar
                   -- commands with fields will pass off to their own dedicated parser
                   return $ case name of
                     "ping" -> Ping
                     "info" -> Info
