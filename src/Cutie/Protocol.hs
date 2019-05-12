{-# Language OverloadedStrings #-}

module Cutie.Protocol where

import qualified Data.ByteString as B
import Data.Void (Void)

import Text.Megaparsec
import Text.Megaparsec.Char

import Data.ByteString.Char8 (pack, unpack)
import Data.Either.Combinators (rightToMaybe)
import Data.Char
import Data.List

-- Only support what we need to here, no need to get fancy

-- The Maybe bit here is arguably a bit unnecessary, but adds to clarity
data Message = Message (Maybe [Tag]) (Maybe Prefix) Command
  deriving (Show)

-- TODO: Make the types for all of these more indicative of the actual
-- meaning of the field -- a big one is a "Nick" type, or something
-- like that, that's actually just a String, but y'know
data Tag = LabelTag String -- draft
         | BatchTag String
         | UnknownTag String (Maybe String)
  deriving (Show)

-- Possibly should be cleaned up a bit, move the second bit out to
-- "userdata" or something?
data Prefix = Prefix String (Maybe String) (Maybe String)
  deriving (Show)

data Command = Nick String (Maybe Int)
             | User String String String String
             | PrivMsg String String
             | Quit String
             | Join [String] (Maybe [String])
             | Part [String]
             -- This is probably a bit off, I should turn it into a
             -- proper subcommand instead of all of these.
             | CapabilityRequest [Capability]
             | CapabilityEnd
             | CapabilityAck [Capability]
             | CapabilityNak [Capability]
             | Identified -- Not a real command, but this is what
             -- we're calling 001. Lets us run a command after we've
             -- identified to the server and it's been accepted
             | Ping String
             | Pong String
             | UnknownCommand String
  deriving (Show)

data Capability = LabeledResponseCapability -- draft
                | BatchCapability
                | UnknownCapability String
  deriving (Show)


type Parser = Parsec Void String

-- Worth noting that this parser is extremely standard non-conforming,
-- there almost certainly are edge cases (and possibly even full-blown
-- cases) where it'll break.

-- It's objectively bad to unpack the bytestring to a string, but it's
-- impossible to use char with any ease otherwise, so I'm willing to
-- take the performance hit for now for more readable code.
decode :: B.ByteString -> Maybe Message
decode = rightToMaybe . parse messageParser "irc" . unpack

messageParser :: Parser Message
messageParser = do tags <- optional tagsParser
                   prefix <- optional prefixParser
                   command <- commandParser
                   -- _ <- crlf
                   return $ Message tags prefix command

tagsParser :: Parser [Tag]
tagsParser = do _    <- char '@'
                tags <- many tagParser
                _    <- char ' '
                return tags

keyNameChar = alphaNumChar
              <|> char '-'

-- defined by what it can *not* be in the spec, AKA a massive pain to
-- represent in megaparsec. This is 100% incorrect.  TODO: switch to
-- `satisfy` so it can properly fulfill the specification.
tagValueChar = alphaNumChar
               <|> symbolChar
               <|> char '-'
               <|> char '.'
               <|> char ':'
               

tagParser :: Parser Tag
tagParser = do key   <- some keyNameChar
               value <- optional $ do _ <- char '='
                                      v <- some tagValueChar
                                      return v
               _     <- optional $ char ';'
               return $ case (key, value) of
                 ("draft/label", Just value) -> LabelTag value
                 ("batch", Just value)       -> BatchTag value
                 (key, value)                -> UnknownTag key value
                 

nickChar = alphaNumChar
           <|> symbolChar
           <|> char '-'
           <|> char '.'
           <|> char ':'

-- Literally defined by `<nonwhite> [<nonwhite>]`, but it obviously
-- can't be @ because that's the next separator...
userChar = nickChar

-- IDK
hostChar = nickChar

prefixParser :: Parser Prefix
prefixParser = do _ <- char ':'
                  nick <- some nickChar
                  user <- optional $ do _ <- char '!'
                                        u <- some userChar
                                        return u
                  host <- optional $ do _ <- char '@'
                                        h <- some hostChar
                                        return h
                  _ <- char ' '
                  return $ Prefix nick user host

commandParser :: Parser Command
commandParser = do keyword <- some alphaNumChar
                   _ <- some spaceChar
                   command <- case (map toLower keyword) of
                                "nick" -> parseNick
                                "user" -> parseUser
                                "privmsg" -> parsePrivMsg
                                -- "join" -> parseJoin
                                "ping" -> parsePing
                                "cap" -> parseCap
                                "001" -> return Identified
                                k -> return $ UnknownCommand k
                   _ <- optional $ some printChar
                   return command

notSpaceChar = satisfy $ (not . isSpace)

-- FIXME: broken, and only designed to notice CAP * ACK
parseCap :: Parser Command
parseCap = do _ <- some alphaNumChar
              _ <- some spaceChar
              second <- some alphaNumChar
              _ <- some spaceChar
              return $ case (map toLower second) of
                "ack" -> CapabilityAck []
                k -> UnknownCommand k

-- This is wrong, but should work for properly formed ping commands
parsePing :: Parser Command
parsePing = do _ <- optional $ char ':'
               to <- some printChar
               return $ Ping to

-- There's some way to abstract over these and I know it, but I'm not
-- gonna worry about it for now. 
parseNick :: Parser Command
parseNick = do nick <- some notSpaceChar
               return $ Nick nick Nothing

parseUser :: Parser Command
parseUser = do nick <- some notSpaceChar
               _    <- some spaceChar
               _1   <- some notSpaceChar
               _    <- some spaceChar
               _2   <- some notSpaceChar
               _    <- some spaceChar
               _3   <- some notSpaceChar
               return $ User nick _1 _2 _3

parsePrivMsg :: Parser Command
parsePrivMsg = do channel <- some notSpaceChar
                  _ <- some (spaceChar <|> char ':') -- FIXME
                  message <- some printChar
                  return $ PrivMsg channel message

-- Does no checking for you encoding nonsense, be careful
encode :: Message -> B.ByteString
encode (Message tags prefix command) =
  pack $ concat [encodeTags tags,
                 encodePrefix prefix,
                 encodeCommand command,
                 "\r\n"]

encodeTags :: Maybe [Tag] -> String
encodeTags (Just tags) = "@" ++ (intercalate ";" $ map encodeTag tags) ++ " "
encodeTags Nothing = ""

encodeTag :: Tag -> String
encodeTag (LabelTag val) = "draft/label=" ++ val

-- ignore everything other than nick for now
encodePrefix :: Maybe Prefix -> String
encodePrefix (Just (Prefix nick mUser mHost)) = ":" ++ nick
encodePrefix Nothing = ""

encodeCommand :: Command -> String
encodeCommand (Join channels Nothing) = "JOIN " ++ (intercalate "," channels)
encodeCommand (CapabilityRequest caps) = "CAP REQ :" ++ (intercalate " " $
                                                         map encodeCapability caps)
encodeCommand (PrivMsg channel message) = "PRIVMSG " ++ channel ++ " :" ++ message
encodeCommand (Pong to) = "PONG :" ++ to
encodeCommand _ = ""

encodeCapability :: Capability -> String
encodeCapability LabeledResponseCapability = "draft/labeled-response"
encodeCapability BatchCapability = "batch"
encodeCapability _ = ""

msgBuilder :: Command -> Message
msgBuilder = Message Nothing Nothing
