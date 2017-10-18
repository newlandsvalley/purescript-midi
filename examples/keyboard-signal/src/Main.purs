module Main where

import Prelude (Unit, bind, (<>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)

import Data.Midi.WebMidi
import Data.Maybe (Maybe(..))
import Data.Midi (TimedEvent(..)) as Midi

import Signal.Channel (CHANNEL, subscribe)
import Signal (Signal, runSignal, (~>))

logDevice :: ∀ eff. Device -> Eff (console :: CONSOLE | eff) Unit
logDevice d =
  let
    connectState =
      if d.connected then
        " has connected"
      else
        " has disconnected"
  in
    log ("MIDI device: " <> d.id <> connectState)

logMidiEvent :: ∀ eff. Midi.TimedEvent -> Eff (console :: CONSOLE | eff) Unit
logMidiEvent (Midi.TimedEvent e) =
  case e.event of
    Just mev -> logShow mev
    Nothing -> log "event parse error "

logEventEffect :: forall eff. Signal Midi.TimedEvent -> Signal (Eff (console :: CONSOLE | eff) Unit)
logEventEffect s = s ~> logMidiEvent

logDeviceEffect :: forall eff. Signal Device -> Signal (Eff (console :: CONSOLE | eff) Unit)
logDeviceEffect s = s ~> logDevice

-- | test web-midi event recognition
testEvents :: forall eff. Eff (wm :: WEBMIDI, console :: CONSOLE, channel :: CHANNEL | eff) Unit
testEvents = do
  -- create a channel for MIDI event messages and start
  -- feeding it from web-midi events (first message is a dummy)
  eChannel <- createEventChannel
  -- subscribe to the channel
  let
    eventSignal = subscribe eChannel
  -- and finally log the messages
  runSignal (logEventEffect eventSignal)

-- | test web-midi device connectipon disconnection
testDevices :: forall eff. Eff (wm :: WEBMIDI, console :: CONSOLE, channel :: CHANNEL | eff) Unit
testDevices = do
  -- create a channel for MIDI device connections and disconnections and start
  dChannel <- createDeviceChannel
  -- subscribe to the channel
  let
    deviceSignal = subscribe dChannel
  -- and finally log the messages
  runSignal (logDeviceEffect deviceSignal)

main :: forall eff. Eff (wm :: WEBMIDI, console :: CONSOLE, channel :: CHANNEL | eff) Unit
main = do
  -- connect to web-midi
  _ <- webMidiConnect
  -- test the devices
  _ <- testDevices
  -- and test the events
  testEvents
