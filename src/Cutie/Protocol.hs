module Cutie.Protocol where

-- Only support what we need to here, no need to get fancy

data Message = Message [Tag] Prefix Command

-- TODO: Make the types for all of these more indicative of the actual
-- meaning of the field -- a big one is a "Nick" type, or something
-- like that, that's actually just a String, but y'know
data Tag = Label String -- draft
         | Batch String
         | UnknownTag String String

data Prefix = Prefix String String String

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

