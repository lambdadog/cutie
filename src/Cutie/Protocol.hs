module Cutie.Protocol where

import qualified Data.ByteString as B

-- Only support what we need to here, no need to get fancy

-- The Maybe bit here is arguably a bit unnecessary, but adds to clarity
data Message = Message (Maybe [Tag]) Prefix Command

-- TODO: Make the types for all of these more indicative of the actual
-- meaning of the field -- a big one is a "Nick" type, or something
-- like that, that's actually just a String, but y'know
data Tag = Label String -- draft
         | Batch String
         | UnknownTag String String

-- Possibly should be cleaned up a bit, move the second bit out to
-- "userdata" or something?
data Prefix = Prefix String (Maybe String) (Maybe String)

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
             | UnknownCommand String

data Capability = LabeledResponseCapability -- draft
                | BatchCapability
                | UnknownCapability String

-- The magic function that turns a string into this mess above. It's
-- fake for now instead of undefined just so the program stays
-- functioning.
decode :: B.ByteString -> Message
decode _ = Message Nothing (Prefix "" Nothing Nothing) (UnknownCommand "")

encode :: Message -> B.ByteString
encode = undefined
