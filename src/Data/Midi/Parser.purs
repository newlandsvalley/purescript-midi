module Data.Midi.Parser
        ( normalise
        , parse
        , parseMidiEvent
        , translateRunningStatus
        ) where

import Prelude (Unit, unit, ($), (<$>), (<*>), (*>), (+), (-), (>),
                (==), (>=), (<=), (&&), (>>=), (>>>), (<<<), map, pure, show, void)
import Control.Alt ((<|>))
import Data.List (List(..), (:), foldl, reverse)
import Data.Foldable (fold)
import Data.Unfoldable (replicateA)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..), fst, snd)
import Data.Newtype (unwrap, wrap)
import Data.Char (fromCharCode, toCharCode)
import Data.String (singleton, fromCharArray, toCharArray)
import Data.Int (pow)
import Data.Int.Bits (and, shl)
import Text.Parsing.StringParser (Parser, runParser, try)
import Text.Parsing.StringParser.String (anyChar, satisfy, string, char, noneOf)
import Text.Parsing.StringParser.Combinators (choice, many, many1Till, (<?>))

import Data.Midi

import Debug.Trace (trace)

{-
traceParse :: forall a. String -> a -> a
traceParse s p =
  trace s (\_ -> p)
-}

traceEvent :: Event -> Event
traceEvent p =
  trace (show p) (\_ -> p)

-- low level parsers

-- | Apply a parser and skip its result.
skip :: forall a. Parser a -> Parser Unit
skip = void

-- | Parse `n` occurences of `p`. -
count :: forall a. Int -> Parser a -> Parser (List a)
count = replicateA

{- parse a binary 8 bit integer -}
int8 :: Parser Int
int8 =
  toCharCode <$> anyChar

{- parse a signed binary 8 bit integer -}
signedInt8 :: Parser Int
signedInt8 =
  (\i ->
    if (topBitSet i) then
      i - 256
    else
      i
  )
    <$> int8

{- parse a specific binary 8 bit integer -}
bchar :: Int -> Parser Int
bchar val =
  toCharCode <$> char (fromCharCode (val))

{- parse an 8 bit integer lying within a range -}
brange :: Int -> Int -> Parser Int
brange l r =
  let
    f a =
      toCharCode a >= l && toCharCode a <= r
  in
    toCharCode <$> satisfy f

{- parse a choice between a pair of 8 bit integers -}
bchoice :: Int -> Int -> Parser Int
bchoice x y =
  bchar x <|> bchar y


notTrackEnd :: Parser Int
notTrackEnd =
  let
    c =
      fromCharCode 0x2F
  in
    toCharCode <$> noneOf [ c ]

-- fixed length integers
int16 :: Parser Int
int16 =
  let
    toInt16 a b =
    -- shiftLeft a 8 + b
    -- shiftLeftBy 8 a + b
      a `shl` 8 + b
  in
    toInt16 <$> int8 <*> int8


int24 :: Parser Int
int24 =
  let
    toInt24 a b c =
      -- shiftLeft a 16 + shiftLeft b 8 + c
      -- shiftLeftBy 16 a + shiftLeftBy 8 b + c
      a `shl` 16 + b `shl` 8 + c
  in
    toInt24 <$> int8 <*> int8 <*> int8

int32 :: Parser Int
int32 =
  let
    toInt32 a b c d =
      -- shiftLeft a 24 + shiftLeft b 16 + shiftLeft c 8 + d
      -- shiftLeftBy 24 a + shiftLeftBy 16 b + shiftLeftBy 8 c + d
      a `shl` 24 + b `shl` 16 + c `shl` 8 + d
  in
    toInt32 <$> int8 <*> int8 <*> int8 <*> int8

-- variable length integers
-- (need to somehow check this for lengths above 16 bits)
varInt :: Parser Int
varInt =
  int8
    >>= (\n ->
          if (topBitSet n) then
            ((+) ((clearTopBit >>> shiftLeftSeven) n)) <$> varInt
          else
            -- succeed n
            pure n
        )


{- just for debug purposes - consume the rest of the input -}
rest :: Parser (List Char)
rest =
  many anyChar

-- top level parsers

midi :: Parser Recording
midi =
  -- midiHeader `andThen` midiTracks
  -- midiHeader |> andThen midiTracks
  midiHeader >>= midiTracks

{- this version of the top level parser just parses many tracks
      without checking whether the track count agrees with the header
   midi0 : Parser s MidiRecording
   midi0 = (,) <$> midiHeader <*> midiTracks0

   midiTracks0 : Parser s (List Track)
   midiTracks0 = many1 midiTrack <?> "midi tracks"
-}
{- simple parser for headers which assumes chunk size is 6
   midiHeader : Parser Header
   midiHeader = string "MThd"
                  *> int32
                  *> ( Header <$>  int16 <*> int16 <*> int16 )
                  <?> "header"
-}
{- parser for headers which quietly eats any extra bytes if we have a non-standard chunk size -}


midiHeader :: Parser Header
midiHeader =
  string "MThd"
    *> let
         h =
            headerChunk <$> int32 <*> int16 <*> int16 <*> int16
       in
         consumeOverspill h 6
            <?> "header"


midiTracks :: Header -> Parser Recording
midiTracks h =
  buildRecording h <$> count (unwrap h).trackCount midiTrack <?> "midi tracks"

{- we don't place TrackEnd events into the parse tree - there is no need.
   The end of the track is implied by the end of the event list
-}
midiTrack :: Parser Track
midiTrack =
  Track <$>
     --(string "MTrk" *> int32 *> many1 midiMessage <* trackEndMessage) <?> "midi track"
     (string "MTrk" *> int32 *> many1Till midiMessage trackEndMessage <?> "midi track")


-- Note - it is important that runningStatus is placed last because of its catch-all definition
midiMessage :: Parser Message
midiMessage =
  Message
    <$> varInt
    <*> midiEvent

midiEvent :: Parser Event
midiEvent =
  -- traceEvent <$>
    choice
      [ metaEvent
      , noteOn
      , noteOff
      , noteAfterTouch
      , controlChange
      , programChange
      , channelAfterTouch
      , pitchBend
      , runningStatus
      ]
        <?> "midi message"

-- metadata parsers
metaEvent :: Parser Event
metaEvent =
   bchar 0xFF
    *> choice
      [ sequenceNumber
      , text
      , copyright
      , trackName
      , instrumentName
      , lyrics
      , marker
      , cuePoint
      , channelPrefix
      , tempoChange
      , smpteOffset
      , timeSignature
      , keySignature
      , sequencerSpecific
      , sysEx
      , unspecified
      ]
        <?> "meta event"

sequenceNumber :: Parser Event
sequenceNumber =
  SequenceNumber <$> (bchar 0x00 *> bchar 0x02 *> int16 <?> "sequence number")

{- parse a simple string-valued meta event -}
parseMetaString :: Int -> Parser String
parseMetaString target =
  catChars
    -- <$> (bchar target *> varInt `andThen` (\l -> count l anyChar))
    <$>
      (bchar target *> varInt >>= (\l -> count l anyChar))

text :: Parser Event
text =
  Text <$> parseMetaString 0x01 <?> "text"

copyright :: Parser Event
copyright =
  Copyright <$> parseMetaString 0x02 <?> "copyright"

trackName :: Parser Event
trackName =
  TrackName <$> parseMetaString 0x03 <?> "track name"


instrumentName :: Parser Event
instrumentName =
  InstrumentName <$> parseMetaString 0x04 <?> "instrument name"

lyrics :: Parser Event
lyrics =
  Lyrics <$> parseMetaString 0x05 <?> "lyrics"

marker :: Parser Event
marker =
  Marker <$> parseMetaString 0x06 <?> "marker"

cuePoint :: Parser Event
cuePoint =
  CuePoint <$> parseMetaString 0x07 <?> "cue point"

channelPrefix :: Parser Event
channelPrefix =
  ChannelPrefix <$> (bchar 0x20 *> bchar 0x01 *> int8 <?> "channel prefix")

tempoChange :: Parser Event
tempoChange =
  Tempo <$> (bchar 0x51 *> bchar 0x03 *> int24) <?> "tempo change"

smpteOffset :: Parser Event
smpteOffset =
  bchar 0x54 *> bchar 0x03 *> (SMPTEOffset <$> int8 <*> int8 <*> int8 <*> int8 <*> int8 <?> "SMTPE offset")

timeSignature :: Parser Event
timeSignature =
  bchar 0x58 *> bchar 0x04 *> (buildTimeSig <$> int8 <*> int8 <*> int8 <*> int8) <?> "time signature"

keySignature :: Parser Event
keySignature =
  bchar 0x59 *> bchar 0x02 *> (KeySignature <$> signedInt8 <*> int8)

sequencerSpecific :: Parser Event
sequencerSpecific =
  SequencerSpecific <$> parseMetaString 0x7F <?> "sequencer specific"

sysEx :: Parser Event
sysEx =
  -- SysEx <$> (String.fromList <$> (bchoice 0xF0 0xF7 *> varInt `andThen` (\l -> count l anyChar))) <?> "system exclusive"
  SysEx <$> (catChars <$> (bchoice 0xF0 0xF7 *> varInt >>= (\l -> count l anyChar))) <?> "system exclusive"

{- parse an unspecified meta event
   The possible range for the type is 00-7F. Not all values in this range are defined, but programs must be able
   to cope with (ie ignore) unexpected values by examining the length and skipping over the data portion.
   We cope by accepting any value here except TrackEnd which is the terminating condition for the list of MidiEvents
   and so must not be recognized here
-}
unspecified :: Parser Event
unspecified =
  -- Unspecified <$> notTrackEnd <*> (int8 `andThen` (\l -> count l int8))
  Unspecified <$> notTrackEnd <*> (int8 >>= (\l -> count l int8))

trackEndMessage :: Parser Unit
trackEndMessage =
    try
      (varInt *> bchar 0xFF *> bchar 0x2F *> bchar 0x00 *> pure unit <?> "track end")

-- channel parsers

noteOn :: Parser Event
noteOn =
  buildNote <$> brange 0x90 0x9F <*> int8 <*> int8 <?> "note on"

noteOff :: Parser Event
noteOff =
  buildNoteOff <$> brange 0x80 0x8F <*> int8 <*> int8 <?> "note off"

noteAfterTouch :: Parser Event
noteAfterTouch =
  buildNoteAfterTouch <$> brange 0xA0 0xAF <*> int8 <*> int8 <?> "note after touch"

controlChange :: Parser Event
controlChange =
  buildControlChange <$> brange 0xB0 0xBF <*> int8 <*> int8 <?> "control change"

programChange :: Parser Event
programChange =
  buildProgramChange <$> brange 0xC0 0xCF <*> int8 <?> "program change"

channelAfterTouch :: Parser Event
channelAfterTouch =
  buildChannelAfterTouch <$> brange 0xD0 0xDF <*> int8 <?> "channel after touch"

pitchBend :: Parser Event
pitchBend =
  buildPitchBend <$> brange 0xE0 0xEF <*> int8 <*> int8 <?> "pitch bend"

{- running status is somewhat anomalous.  It inherits the 'type' of the last event parsed, which must be a channel event.
   This inherited channel event type is not put into the parse tree - this is left to an interpreter
-}
runningStatus :: Parser Event
runningStatus =
  RunningStatus <$> brange 0x00 0x7F <*> int8 <?> "running status"

-- runningStatus = log "running status" <$> ( RunningStatus <$> brange 0x00 0x7F  <*> int8 <?> "running status")
-- result builder
{- build a Header and make the chunk length available so that any overspill bytes can
   later be quietly ignored
-}
headerChunk :: Int -> Int -> Int -> Int -> Tuple Int Header
headerChunk l a b c =
  let
    header =
        { formatType : a
        , trackCount : b
        , ticksPerBeat : c
        }
  in
    Tuple l (Header header)

buildRecording :: Header -> List Track -> Recording
buildRecording h ts =
  Recording { header: h, tracks : ts }

{- build NoteOn (unless the velocity is zero in which case NoteOff) -}
buildNote :: Int -> Int -> Int -> Event
buildNote cmd note velocity =
  let
    channel =
      -- cmd `and` 0x0F
      and cmd 0x0F

    isOff =
      (velocity == 0)
  in
    case isOff of
      true ->
        NoteOff channel note velocity

      _ ->
        NoteOn channel note velocity

{- abstract builders that construct MidiEvents that all have the same shape -}
channelBuilder3 :: (Int -> Int -> Int -> Event) -> Int -> Int -> Int -> Event
channelBuilder3 construct cmd x y =
  let
    channel =
      -- cmd `and` 0x0F
      and cmd 0x0F
  in
    construct channel x y

channelBuilder2 :: (Int -> Int -> Event) -> Int -> Int -> Event
channelBuilder2 construct cmd x =
  let
    channel =
      -- cmd `and` 0x0F
      and cmd 0x0F
  in
    construct channel x

{- build NoteOff -}
buildNoteOff :: Int -> Int -> Int -> Event
buildNoteOff cmd note velocity =
  channelBuilder3 NoteOff cmd note velocity

{- build Note AfterTouch AKA Polyphonic Key Pressure -}
buildNoteAfterTouch :: Int -> Int -> Int -> Event
buildNoteAfterTouch cmd note pressure =
  channelBuilder3 NoteAfterTouch cmd note pressure

{- build Control Change -}
buildControlChange :: Int -> Int -> Int -> Event
buildControlChange cmd num value =
  channelBuilder3 ControlChange cmd num value

{- build Program Change -}
buildProgramChange :: Int -> Int -> Event
buildProgramChange cmd num =
  channelBuilder2 ProgramChange cmd num

{- build Channel AfterTouch AKA Channel Key Pressure -}
buildChannelAfterTouch :: Int -> Int -> Event
buildChannelAfterTouch cmd num =
  channelBuilder2 ChannelAfterTouch cmd num

{- build Pitch Bend -}
buildPitchBend :: Int -> Int -> Int -> Event
buildPitchBend cmd lsb msb =
  channelBuilder2 PitchBend cmd $ lsb + shiftLeftSeven msb

{- build a Time Signature -}
buildTimeSig :: Int -> Int -> Int -> Int -> Event
buildTimeSig nn dd cc bb =
  let
    denom =
      2 `pow` dd
  in
    TimeSignature nn denom cc bb

-- utility functions
{- consume the overspill from a non-standard size chunk
   actual is the parsed actual chunk size followed by the chunk contents (which are returned)
   expected is the expected size of the chunk
   consume the rest if the difference suggests an overspill of unwanted chunk material
-}
consumeOverspill :: forall a. Parser (Tuple Int a ) -> Int -> Parser a
consumeOverspill actual expected =
  actual
    >>= (\(Tuple cnt rest ) ->
          map (\_ -> rest) $
                skip $
                    count (cnt - expected) int8
        )

topBitSet :: Int -> Boolean
topBitSet n =
  -- n `and` 0x80 > 0
  and n 0x80 > 0

clearTopBit :: Int -> Int
clearTopBit n =
  -- n `and` 0x7F
  and n 0x7F

shiftLeftSeven :: Int -> Int
shiftLeftSeven n =
    -- shiftLeft n 7
    n `shl` 7

makeTuple :: forall a b. a -> b -> Tuple a b
makeTuple a b =
  Tuple a b

-- utils
catChars :: List Char -> String
catChars = fold <<< map singleton

translateNextEvent :: Tuple Event Track -> Message -> Tuple  Event Track
translateNextEvent acc nextMessage =
  let
    -- ( state, events ) =  acc
    state = fst acc
    events = unwrap $ snd acc

        -- ( ticks, next ) =  nextMessage
  in
    case nextMessage of
      Message ticks (RunningStatus x y) ->
        let
          translatedStatus =
            interpretRS state x y
        in
          case translatedStatus of
            Unspecified _ _ ->
              -- couldn't translate the running status so drop it
              Tuple state (wrap events)

            _ ->
              -- could translate the running status so adopt it
              Tuple state ( Track ((Message ticks translatedStatus) : events) )

      Message _ other ->
        Tuple other (Track (nextMessage : events))


-- just update the state
{- we can interpret the running status if we have a legitimate last event state which is a channel voice event -}
interpretRS :: Event -> Int -> Int -> Event
interpretRS last x y =
  case last of
    NoteOn chan _ _ ->
      if (y == 0) then
        NoteOff chan x y
      else
        NoteOn chan x y

    NoteOff chan _ _ ->
      NoteOff chan x y

    NoteAfterTouch chan _ _ ->
      NoteAfterTouch chan x y

    ControlChange chan _ _ ->
      ControlChange chan x y

    ProgramChange _ _ ->
      ProgramChange x y

    ChannelAfterTouch _ _ ->
      ChannelAfterTouch x y

    PitchBend _ _ ->
      PitchBend x y

    _ ->
      Unspecified 0 Nil

{-}
translateAllRunningStatus :: Track -> Track
translateAllRunningStatus =
  wrap <<< reverse <<< unwrap <<< snd <<<
    foldl translateNextEvent ( Tuple (Unspecified 0 Nil) (Track Nil) )
    -}

translateAllRunningStatus :: Track -> Track
translateAllRunningStatus wrappedTrack =
  let
    track = unwrap wrappedTrack
    translate =
      (wrap <<< reverse <<< unwrap <<< snd <<<
        foldl translateNextEvent ( Tuple (Unspecified 0 Nil) (Track Nil) ))
  in
    translate track

-- exported functions

-- | Parse a MIDI event
parseMidiEvent :: String -> Either String Event
parseMidiEvent s =
    case runParser midiEvent s of
        Right n ->
            Right n

        Left e ->
            Left $ show e

{-}
    case parse midiEvent s of
        Ok ( _, _, n ) ->
            Ok n

        Err ( _, ctx, ms ) ->
            Err ("parse error: " ++ (toString ms) ++ ", " ++ (toString ctx))
-}


-- | entry point - Parse a normalised MIDI file image
parse :: String -> Either String Recording
parse s =

    case runParser midi s of
        Right n ->
            Right n

        Left e ->
            Left $ show e
{-}
    case Combine.parse midi s of
        -- case Combine.parse (midi <* end) s of
        -- ( Ok n, _ ) ->
        Ok ( _, _, n ) ->
            Ok n

        -- ( Err ms, cx ) ->
        --    Err ("parse error: " ++ (toString ms) ++ ", " ++ (toString cx))
        Err ( _, ctx, ms ) ->
            Err ("parse error: " ++ (toString ms) ++ ", " ++ (toString ctx))
-}


-- | normalise the input before we parse by masking off all but the least significant 8 bits
normalise :: String -> String
normalise =
    let
        f =
            toCharCode >>> ((and) 0xFF) >>> fromCharCode
    in
        toCharArray >>> map f >>> fromCharArray

-- | translate the Running Status messages in each track to the expanded form (NoteOn/NoteOff etc)
translateRunningStatus :: Either String Recording -> Either String Recording
translateRunningStatus res =
    case res of
        Right mr ->
            let
                tracks =
                    -- second mr |> List.
                    map translateAllRunningStatus (unwrap mr).tracks
            in
                Right (Recording { header : (unwrap mr).header, tracks : tracks })

        err ->
            err
