module Data.Midi.Generate (event, recording) where

-- | Library for encoding MIDI types as "binary"
-- | adapted from the elm-comidi version courtesy of @rhofour

import Prelude ((<>), (+), (<), (<<<), map)
import Data.Int.Bits (shr, and)
import Data.List (List(..), (:), concat, concatMap, fromFoldable, length)
import Data.Char (toCharCode)
import Data.String (toCharArray)
import Data.Midi

event :: Event -> List Byte
event evt =
  case evt of
    SysEx F0 bytes ->
      0xF0 : bytes

    SysEx F7 bytes ->
      bytes

    NoteOn channel note velocity ->
      ( 0x90 + channel : note : velocity : Nil )

    NoteOff channel note velocity ->
      ( 0x80 + channel : note : velocity : Nil )

    NoteAfterTouch channel note velocity ->
      ( 0xA0 + channel : note : velocity : Nil )

    ControlChange channel controllerNumber value ->
      ( 0xB0 + channel : controllerNumber : value : Nil )

    ProgramChange channel value ->
      ( 0xC0 + channel : value : Nil )

    ChannelAfterTouch channel velocity ->
      ( 0xD0 + channel : velocity : Nil )

    PitchBend channel bend ->
      let
        lower =
          and bend 127

        upper =
          shr bend 7
      in
        ( 0xE0 + channel : lower : upper : Nil)

    _ ->
      Nil


recording :: Recording -> List Byte
recording (Recording {header, tracks} ) =
  (head header (length tracks)) <> (concatMap track tracks)



-- Lower level generators


head :: Header -> Int -> List Byte
head (Header h) trackCount =
  let
    format =
      h.formatType

    numTracks =
      trackCount

    division =
      h.ticksPerBeat
  in
    concat
      ( strToBytes "MThd"
      : uint32 6
      : uint16 format
      : uint16 numTracks
      : uint16 division
      : Nil
      )

track :: Track -> List Byte
track (Track t) =
  let
    endOfTrack =
      ( 0x00 : 0xFF : 0x2F : 0x00 : Nil )

    encodedMsgs =
      (concatMap midiMessage t) <> endOfTrack

    len =
      length encodedMsgs
  in
    (strToBytes "MTrk") <> uint32 len <> encodedMsgs


midiMessage :: Message -> List Byte
midiMessage ( Message ticks ev ) =
  (varInt ticks) <> (fileEvent ev)


fileEvent :: Event -> List Byte
fileEvent e =
  case e of
    SysEx F0 bytes ->
      0xF0 : (varInt (length bytes)) <> bytes

    SysEx F7 bytes ->
      0xF7 : (varInt (length bytes)) <> bytes

    _ ->
      -- Use the regular event generator for everything else
      event e



-- Helper functions

strToBytes :: String -> List Byte
strToBytes =
  (map toCharCode) <<< fromFoldable <<< toCharArray


uint16 :: Int -> List Byte
uint16 x =
  let
    b1 =
      and 255 (shr x 8)

    b2 =
      and 255 x
  in
    ( b1 : b2 : Nil )


uint32 :: Int -> List Byte
uint32 x =
  let
    b1 =
      and 255 (shr x 24)

    b2 =
      and 255 (shr x 16)

    b3 =
      and 255 (shr x 8)

    b4 =
      and 255 x
  in
    ( b1 :  b2 :  b3 :  b4 : Nil )


varInt :: Int -> List Byte
varInt x =
  let
    varIntHelper x' bytes =
      if x' < 128 then
        (x' + 128) : bytes
      else
        varIntHelper
          (shr x' 7)
          ((128 + (and 127 x')) : bytes)
  in
    if x < 128 then
      x : Nil
    else
      varIntHelper (shr x 7) ( and 127 x : Nil )
