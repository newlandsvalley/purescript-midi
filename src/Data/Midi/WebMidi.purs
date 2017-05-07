module Data.Midi.WebMidi
  ( WEBMIDI
  , RawMidiEvent
  , Device
  , webMidiConnect
  , detectInputDevices
  , listen ) where

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (kind Effect, Eff)
import Prelude (Unit)

-- | A Midi Event
type RawMidiEvent =
 { timeStamp :: Number
 , encodedBinary :: String
 }

-- | A Midi Device
type Device =
 { connected :: Boolean
 , portType :: String
 , id :: String
 , manufacturer :: String
 , name :: String
 , version :: String
}

-- | WEBMIDI Effect
foreign import data WEBMIDI :: Effect

foreign import listenImpl :: forall e. (RawMidiEvent -> Eff e Unit) -> Eff e Unit

foreign import detectInputDevicesImpl :: forall e. (Device -> Eff e Unit) -> Eff e Unit

-- | try to connect to Web-Midi.
foreign import webMidiConnect
  :: forall eff. (Eff (wm :: WEBMIDI | eff) Boolean)

-- | listen for MIDI messages.
listen :: forall e. Aff e RawMidiEvent
listen = makeAff (\error success -> listenImpl success)

-- | detect connected/connecting/disconnecting input devices.
detectInputDevices :: forall e. Aff e Device
detectInputDevices = makeAff (\error success -> detectInputDevicesImpl success)
