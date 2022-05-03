-- | This module provides plug and play support for MIDI input devices such as
-- | MIDI keyboards.
module Data.Midi.WebMidi
  ( Device
  , RawMidiEvent
  , createDeviceChannel
  , createEventChannel
  , detectInputDevices
  , deviceSignal
  , eventSignal
  , listen
  , webMidiConnect
  )
  where

import Effect (Effect)
import Prelude (Unit, flip, bind, const, pure )
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Midi (TimedEvent(..)) as Midi
import Data.Midi.Parser (parseMidiEvent)
import Signal.Channel (Channel, channel, send)
import Signal

-- | A 'raw' Midi Event where the event has not been decoded.
type RawMidiEvent =
 { timeStamp :: Number
 , encodedBinary :: String
 }

-- | A Midi Device.
type Device =
 { connected :: Boolean
 , portType :: String
 , id :: String
 , manufacturer :: String
 , name :: String
 , version :: String
}

-- | listen to 'raw' web-MIDI event messages.
foreign import listen :: (RawMidiEvent -> Effect Unit) -> Effect Unit

-- | detect any input device as it connects or disconnects
foreign import detectInputDevices :: (Device -> Effect Unit) -> Effect Unit

-- | try to connect to Web-Midi.
foreign import webMidiConnect :: Effect Boolean

-- | convert a raw MIDI event to a comprehensible one by parsing the binary event.
timedMidiEvent :: RawMidiEvent -> Midi.TimedEvent
timedMidiEvent rme =
  case parseMidiEvent rme.encodedBinary of
    Left _er ->
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


deviceChannel :: Effect (Channel Device)
deviceChannel = channel initialDevice

sendDevice :: Channel Device -> Device -> Effect Unit
sendDevice chan d =
  send chan d

-- | create a channel for MIDI device connections/disconnections and feed it from web-midi
createDeviceChannel :: Effect (Channel Device)
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


eventChannel :: Effect (Channel Midi.TimedEvent)
eventChannel = channel initialEvent

sendEvent :: Channel Midi.TimedEvent -> RawMidiEvent -> Effect Unit
sendEvent chan rme =
  send chan (timedMidiEvent rme)

-- | create a channel for MIDI timed event messages and feed it from web-midi
createEventChannel :: Effect (Channel Midi.TimedEvent)
createEventChannel = do
  channel <- eventChannel
  _ <- listen (sendEvent channel)
  pure channel
