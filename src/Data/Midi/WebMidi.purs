module Data.Midi.WebMidi
  ( WEBMIDI
  , RawMidiEvent
  , Device
  , webMidiConnect
  , detectInputDevices
  , listen
  , createDeviceChannel
  , createEventChannel
  ) where

import Control.Monad.Eff (kind Effect, Eff)
import Prelude (Unit, flip, bind, const, pure )
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Midi (TimedEvent(..)) as Midi
import Data.Midi.Parser (parseMidiEvent)
import Signal.Channel (CHANNEL, Channel, channel, send)
import Signal

-- | A 'raw' Midi Event
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

-- | listen to 'raw' web-MIDI event messages
foreign import listen :: forall e. (RawMidiEvent -> Eff e Unit) -> Eff e Unit

-- | detect any input device as it connects or disconnects
foreign import detectInputDevices :: forall e. (Device -> Eff e Unit) -> Eff e Unit

-- | try to connect to Web-Midi.
foreign import webMidiConnect
  :: forall eff. (Eff (wm :: WEBMIDI | eff) Boolean)

-- | convert a Raw MIDI event to a comprehensible one
-- | by parsing the binary event
timedMidiEvent :: RawMidiEvent -> Midi.TimedEvent
timedMidiEvent rme =
  case parseMidiEvent rme.encodedBinary of
    Left er ->
      Midi.TimedEvent {
        timeStamp : rme.timeStamp
      , event     : Nothing
      }
    Right ev ->
      Midi.TimedEvent {
        timeStamp : rme.timeStamp
      , event     : Just ev
      }

initialDevice :: Device
initialDevice =
  { connected : false
  , portType : ""
  , id : ""
  , manufacturer : ""
  , name : ""
  , version : ""
 }

initialDeviceSignal :: Signal Device
initialDeviceSignal =
  constant initialDevice

deviceSignal :: Device -> Signal Device
deviceSignal d =
  foldp (flip const) d initialDeviceSignal

deviceChannel :: ∀ eff. Eff (channel :: CHANNEL | eff) (Channel Device)
deviceChannel = channel initialDevice

sendDevice :: ∀ eff. Channel Device -> Device -> Eff (channel :: CHANNEL | eff) Unit
sendDevice chan d =
  send chan d

-- | create a channel for MIDI device connections/disconnections and feed it from web-midi
createDeviceChannel :: ∀ eff.
  Eff
    ( channel :: CHANNEL
    | eff
    )
    (Channel Device)
createDeviceChannel = do
  channel <- deviceChannel
  _ <- detectInputDevices (sendDevice channel)
  pure channel

initialEvent :: Midi.TimedEvent
initialEvent = Midi.TimedEvent
  { timeStamp : 0.0
  , event : Nothing
  }

initialEventSignal :: Signal  Midi.TimedEvent
initialEventSignal =
  constant initialEvent

eventSignal :: Midi.TimedEvent -> Signal Midi.TimedEvent
eventSignal rme =
  foldp (flip const) rme initialEventSignal

eventChannel :: ∀ eff. Eff (channel :: CHANNEL | eff) (Channel Midi.TimedEvent)
eventChannel = channel initialEvent

sendEvent :: ∀ eff. Channel Midi.TimedEvent -> RawMidiEvent -> Eff (channel :: CHANNEL | eff) Unit
sendEvent chan rme =
  send chan (timedMidiEvent rme)

-- | create a channel for MIDI timed event messages and feed it from web-midi
createEventChannel :: ∀ eff.
  Eff
    ( channel :: CHANNEL
    | eff
    )
    (Channel Midi.TimedEvent)
createEventChannel = do
  channel <- eventChannel
  _ <- listen (sendEvent channel)
  pure channel
