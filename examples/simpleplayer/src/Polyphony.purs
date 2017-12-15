module Polyphony (toPolyphonicPerformance) where

import Audio.SoundFont (MidiNote)
import MidiPerformance (toPerformance)
import Data.Midi (Header(..), Recording(..))
import Data.List (List(..), (:))
import Prelude ((<), (-))

-- | merge the performance from each MIDI track into a single
-- | polyphonic performance
toPolyphonicPerformance :: Recording -> List MidiNote
toPolyphonicPerformance (Recording recording) =
  let
    (Header header ) = recording.header
  in
    mergeTrackFrom (Recording recording) (header.trackCount - 1) Nil

merge :: List MidiNote -> List MidiNote -> List MidiNote
merge Nil ns2 =  ns2
merge ns1 Nil =  ns1
merge a@(n1 : ns1)  b@(n2 : ns2)  =
  if n1.timeOffset < n2.timeOffset then
    n1 : merge ns1 b
  else
    n2 : merge a ns2

mergeTrack :: Recording -> Int -> List MidiNote -> List MidiNote
mergeTrack recording trackNo acc =
  let
    nextTrack = toPerformance recording trackNo
  in
    merge nextTrack acc

mergeTrackFrom :: Recording -> Int -> List MidiNote -> List MidiNote
mergeTrackFrom recording trackNo acc =
  case
    trackNo of
      0 ->
        mergeTrack recording 0 acc
      n ->
        let
          nextAcc = mergeTrack recording n acc
        in
          mergeTrackFrom recording (n - 1) nextAcc
