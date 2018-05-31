
-- | Encode MIDI types as "binary"
-- | adapted from the elm-comidi version courtesy of @rhofour.
module Data.Midi.Generate (Context(..), event, recording, midiMessage) where

import Data.Midi

import Data.Char (toCharCode)
import Data.Int.Bits (shr, and)
import Data.List (List(..), (:), concat, concatMap, fromFoldable, length)
import Data.List.NonEmpty (toList)
import Data.String (length) as Str
import Data.String.CodeUnits (toCharArray)
import Prelude (map, ($), (+), (<), (<<<), (<=), (<>))

-- | The MIDI generation context. This differs according to whether we're
-- | generating a file or an Event stream.
data Context =
    File
  | Stream

-- | Generate a MIDI event.
event :: Context -> Event -> List Byte
event ctx evt =
  case evt of

    SysEx F0 nelBytes ->
      let
        bytes = toList nelBytes
      in
        case ctx of
          File ->
            0xF0 : (varInt (length bytes)) <> bytes
          _ ->
            0xF0 : bytes

    -- this is still not entirely clear
    SysEx F7 nelBytes ->
      let
        bytes = toList nelBytes
      in
        case ctx of
          File ->
            0xF7 : (varInt (length bytes)) <> bytes
          _ ->
            -- not ideal.  Stream SysEx only uses the F0 flavour
            0xF0 : bytes

    -- channel events
    NoteOn channel note velocity ->
      (0x90 + channel) : note : velocity : Nil

    NoteOff channel note velocity ->
      (0x80 + channel) : note : velocity : Nil

    NoteAfterTouch channel note velocity ->
      (0xA0 + channel) : note : velocity : Nil

    ControlChange channel controllerNumber value ->
      (0xB0 + channel) : controllerNumber : value : Nil

    ProgramChange channel value ->
      (0xC0 + channel) : value : Nil

    ChannelAfterTouch channel velocity ->
      ( 0xD0 + channel) : velocity : Nil

    PitchBend channel bend ->
      let
        lower =
          and bend 127

        upper =
          shr bend 7
      in
        (0xE0 + channel) : lower : upper : Nil

    -- meta events
    SequenceNumber seq ->
       ( 0xFF : 0x00 : 0x02 : uint16 seq )

    Text text ->
      ( 0xFF : 0x01 : vstrToBytes text)

    Copyright text ->
      ( 0xFF : 0x02 : vstrToBytes text)

    TrackName text ->
      ( 0xFF : 0x03 : vstrToBytes text)

    InstrumentName text ->
      ( 0xFF : 0x04 : vstrToBytes text)

    Lyrics text ->
      ( 0xFF : 0x05 : vstrToBytes text)

    Marker text ->
      ( 0xFF : 0x06 : vstrToBytes text)

    CuePoint text ->
      ( 0xFF : 0x07 : vstrToBytes text)

    Tempo t ->
      ( 0xFF : 0x51 : 0x03 : uint24 t)

    SMPTEOffset hr mn se fr ff ->
      ( 0xFF : 0x54 : 0x03 : hr : mn : se : fr : ff : Nil)

    TimeSignature nn dd cc bb ->
      ( 0xFF : 0x58 : 0x04 : nn : (pseudoLog dd) : cc : bb : Nil)

    KeySignature sf mi ->
      ( 0xFF : 0x59 : 0x02 : (signedInt8 sf) : mi : Nil)

    SequencerSpecific bytes ->
      ( 0xFF : 0x7F : vBytes bytes)

    -- not really to be used other than in testing
    Unspecified code bytes ->
      ( 0xFF : code : vBytes bytes)

    _ ->
      Nil

-- | Generate a MIDI recording
recording :: Recording -> List Byte
recording (Recording {header, tracks} ) =
  (head header (length tracks)) <> (concatMap track tracks)

-- | generate a MIDI message
midiMessage :: Message -> List Byte
midiMessage ( Message ticks ev ) =
  (varInt ticks) <> (event File ev)


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


{-
fileEvent :: Event -> List Byte
fileEvent e =
  case e of
    SysEx F0 bytes ->
      0xF0 : (varInt (length bytes)) <> bytes

    SysEx F7 bytes ->
      0xF7 : (varInt (length bytes)) <> bytes

    _ ->
      -- Use the regular event generator for everything else
      event File e
-}



-- Helper functions

-- fixed length strings
strToBytes :: String -> List Byte
strToBytes =
  (map toCharCode) <<< fromFoldable <<< toCharArray

-- variable length strings
vstrToBytes :: String -> List Byte
vstrToBytes s =
  (varInt $ Str.length s) <> (strToBytes s)

vBytes :: List Byte -> List Byte
vBytes bytes =
  (varInt $ length bytes) <> bytes

uint16 :: Int -> List Byte
uint16 x =
  let
    b1 =
      and 255 (shr x 8)

    b2 =
      and 255 x
  in
    ( b1 : b2 : Nil )

uint24 :: Int -> List Byte
uint24 x =
  let
    b1 =
      and 255 (shr x 16)

    b2 =
      and 255 (shr x 8)

    b3 =
      and 255 x
  in
    ( b1 :  b2 :  b3 :  Nil )

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

signedInt8 :: Int -> Int
signedInt8 x =
  and 255 x

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

-- this messy little routine is intended to convert 2^n to n
-- for 0 <= n <= 7
-- i.e. convert a time signature (e.g. the 4 in 3/4) to a power of 2
-- I want to avoid the dependency on purescript-decimals which has an
-- awkward transitive dependency on decimal.js
pseudoLog :: Int -> Int
pseudoLog x =
  if (x <= 1) then 0
  else if (x <= 2) then 1
  else if (x <= 4) then 2
  else if (x <= 8) then 3
  else if (x <= 16) then 4
  else if (x <= 32) then 5
  else if (x <= 64) then 6
  else 7
