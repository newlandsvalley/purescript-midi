module Main where

import Prelude (Unit, bind, (<>))
import Effect (Effect)
import Effect.Console (log, logShow)

import Data.Midi.WebMidi
import Data.Maybe (Maybe(..))
import Data.Midi (TimedEvent(..)) as Midi

import Signal.Channel (subscribe)
import Signal (Signal, runSignal, (~>))

logDevice :: Device -> Effect Unit
logDevice d =
  let
    connectState =
      if d.connected then
        " has connected"
      else
        " has disconnected"
  in
    log ("MIDI device: " <> d.id <> connectState)

logMidiEvent :: Midi.TimedEvent -> Effect Unit
logMidiEvent (Midi.TimedEvent e) =
  case e.event of
    Just mev -> logShow mev
    Nothing -> log "event parse error "

logEventEffect :: Signal Midi.TimedEvent -> Signal (Effect Unit)
logEventEffect s = s ~> logMidiEvent

logDeviceEffect :: Signal Device -> Signal (Effect Unit)
logDeviceEffect s = s ~> logDevice

-- | test web-midi event recognition
testEvents :: Effect Unit
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
testDevices :: Effect Unit
testDevices = do
  -- create a channel for MIDI device connections and disconnections and start
  dChannel <- createDeviceChannel
  -- subscribe to the channel
  let
    deviceSignal = subscribe dChannel
  -- and finally log the messages
  runSignal (logDeviceEffect deviceSignal)

main :: Effect Unit
main = do
  -- connect to web-midi
  _ <- webMidiConnect
  -- test the devices
  _ <- testDevices
  -- and test the events
  testEvents
