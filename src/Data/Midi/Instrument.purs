module Data.Midi.Instrument
  ( InstrumentName(..)
  , instrumentNames
  , gleitzmanName
  , gleitzmanNames
  , readGleitzman) where

-- | An enumeration of the virtual instruments defined by MIDI together with a
-- | mapping to the instrument names defined in Benjamin Gleitzman's soundfont
-- | library - https://github.com/gleitz/midi-js-soundfonts.
-- | See the MIDI specification, page 150 - the General MIDI Sound Set.

import Data.Array (cons, drop, elemIndex, index, length, filter)
import Data.Foldable (foldr)
import Data.List (List(..), (:))
import Data.Map (Map, fromFoldable, lookup)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Tuple (Tuple(..), fst, snd)
import Prelude (class Eq, class Ord, class Show, compare, eq, map, ($), (<<<), (==), (>))

-- | MIDI instrument names
data InstrumentName =
    Accordion           | AcousticBass        | AcousticGrandPiano
  | AcousticGuitarNylon | AcousticGuitarSteel | Agogo
  | AltoSax             | Applause            | Bagpipe
  | Banjo               | BaritoneSax         | Bassoon
  | BirdTweet           | BlownBottle         | BrassSection
  | BreathNoise         | BrightAcousticPiano | Celesta
  | Cello               | ChoirAahs           | ChurchOrgan
  | Clarinet            | Clavinet            | Contrabass
  | DistortionGuitar    | DrawbarOrgan        | Dulcimer
  | ElectricBassFinger  | ElectricBassPick    | ElectricGrandPiano
  | ElectricGuitarClean | ElectricGuitarJazz  | ElectricGuitarMuted
  | ElectricPiano1      | ElectricPiano2      | EnglishHorn
  | Fiddle              | Flute               | FrenchHorn
  | FretlessBass        | Fx1Rain             | Fx2Soundtrack
  | Fx3Crystal          | Fx4Atmosphere       | Fx5Brightness
  | Fx6Goblins          | Fx7Echoes           | Fx8Scifi
  | Glockenspiel        | GuitarFretNoise     | GuitarHarmonics
  | Gunshot             | Harmonica           | Harpsichord
  | Helicopter          | HonkytonkPiano      | Kalimba
  | Koto                | Lead1Square         | Lead2Sawtooth
  | Lead3Calliope       | Lead4Chiff          | Lead5Charang
  | Lead6Voice          | Lead7Fifths         | Lead8BassLead
  | Marimba             | MelodicTom          | MusicBox
  | MutedTrumpet        | Oboe                | Ocarina
  | OrchestraHit        | OrchestralHarp      | OverdrivenGuitar
  | Pad1NewAge          | Pad2Warm            | Pad3Polysynth
  | Pad4Choir           | Pad5Bowed           | Pad6Metallic
  | Pad7Halo            | Pad8Sweep           | PanFlute
  | PercussiveOrgan     | Piccolo             | PizzicatoStrings
  | Recorder            | ReedOrgan           | ReverseCymbal
  | RockOrgan           | Seashore            | Shakuhachi
  | Shamisen            | Shanai              | Sitar
  | SlapBass1           | SlapBass2           | SopranoSax
  | SteelDrums          | StringEnsemble1     | StringEnsemble2
  | SynthBass1          | SynthBass2          | SynthBrass1
  | SynthBrass2         | SynthChoir          | SynthDrum
  | SynthStrings1       | SynthStrings2       | TaikoDrum
  | TangoAccordion      | TelephoneRing       | TenorSax
  | Timpani             | TinkleBell          | TremoloStrings
  | Trombone            | Trumpet             | Tuba
  | TubularBells        | Vibraphone          | Viola
  | Violin              | VoiceOohs           | Whistle
  | Woodblock           | Xylophone

-- deriving Eq gives an IncompleteExhaustivityCheck warning at about
-- 100 elements in the enumeration and defining it explicitly
-- dramatically reduces compile time
-- deriving Ord is just disastrous in terms of code bloat
-- and both hammer compile time

instance eqInstrumentName :: Eq InstrumentName where
  eq a b = eq (showName a) (showName b)

instance ordInstrumentName :: Ord InstrumentName where
  compare a b = compare (showName a) (showName b)

instance showInstrumentName :: Show InstrumentName where
  show = showName

-- | convert a MIDI instrument name to a Gleitxman string
-- | for example ElectricPiano1 -> electric_piano_1
gleitzmanName :: InstrumentName -> String
gleitzmanName =
  toGleitzmanName <<< showName

-- | Simple show function with a one-to-one correspondence
-- | between the characters that make up the enumerated type
-- | and the String representation.
showName :: InstrumentName -> String
showName inst =
  case inst of
    Accordion -> "Accordion"
    AcousticBass -> "AcousticBass"
    AcousticGrandPiano -> "AcousticGrandPiano"
    AcousticGuitarNylon -> "AcousticGuitarNylon"
    AcousticGuitarSteel -> "AcousticGuitarSteel"
    Agogo -> "Agogo"
    AltoSax -> "AltoSax"
    Applause -> "Applause"
    Bagpipe -> "Bagpipe"
    Banjo -> "Banjo"
    BaritoneSax -> "BaritoneSax"
    Bassoon -> "Bassoon"
    BirdTweet -> "BirdTweet"
    BlownBottle -> "BlownBottle"
    BrassSection -> "BrassSection"
    BreathNoise -> "BreathNoise"
    BrightAcousticPiano -> "BrightAcousticPiano"
    Celesta -> "Celesta"
    Cello -> "Cello"
    ChoirAahs -> "ChoirAahs"
    ChurchOrgan -> "ChurchOrgan"
    Clarinet -> "Clarinet"
    Clavinet -> "Clavinet"
    Contrabass -> "Contrabass"
    DistortionGuitar -> "DistortionGuitar"
    DrawbarOrgan -> "DrawbarOrgan"
    Dulcimer -> "Dulcimer"
    ElectricBassFinger -> "ElectricBassFinger"
    ElectricBassPick -> "ElectricBassPick"
    ElectricGrandPiano -> "ElectricGrandPiano"
    ElectricGuitarClean -> "ElectricGuitarClean"
    ElectricGuitarJazz -> "ElectricGuitarJazz"
    ElectricGuitarMuted -> "ElectricGuitarMuted"
    ElectricPiano1 -> "ElectricPiano1"
    ElectricPiano2 -> "ElectricPiano2"
    EnglishHorn -> "EnglishHorn"
    Fiddle -> "Fiddle"
    Flute -> "Flute"
    FrenchHorn -> "FrenchHorn"
    FretlessBass -> "FretlessBass"
    Fx1Rain -> "Fx1Rain"
    Fx2Soundtrack -> "Fx2Soundtrack"
    Fx3Crystal -> "Fx3Crystal"
    Fx4Atmosphere -> "Fx4Atmosphere"
    Fx5Brightness -> "Fx5Brightness"
    Fx6Goblins -> "Fx6Goblins"
    Fx7Echoes -> "Fx7Echoes"
    Fx8Scifi -> "Fx8Scifi"
    Glockenspiel -> "Glockenspiel"
    GuitarFretNoise -> "GuitarFretNoise"
    GuitarHarmonics -> "GuitarHarmonics"
    Gunshot -> "Gunshot"
    Harmonica -> "Harmonica"
    Harpsichord -> "Harpsichord"
    Helicopter -> "Helicopter"
    HonkytonkPiano -> "HonkytonkPiano"
    Kalimba -> "Kalimba"
    Koto -> "Koto"
    Lead1Square -> "Lead1Square"
    Lead2Sawtooth -> "Lead2Sawtooth"
    Lead3Calliope -> "Lead3Calliope"
    Lead4Chiff -> "Lead4Chiff"
    Lead5Charang -> "Lead5Charang"
    Lead6Voice -> "Lead6Voice"
    Lead7Fifths -> "Lead7Fifths"
    Lead8BassLead -> "Lead8BassLead"
    Marimba -> "Marimba"
    MelodicTom -> "MelodicTom"
    MusicBox -> "MusicBox"
    MutedTrumpet -> "MutedTrumpet"
    Oboe -> "Oboe"
    Ocarina -> "Ocarina"
    OrchestraHit -> "OrchestraHit"
    OrchestralHarp -> "OrchestralHarp"
    OverdrivenGuitar -> "OverdrivenGuitar"
    Pad1NewAge -> "Pad1NewAge"
    Pad2Warm -> "Pad2Warm"
    Pad3Polysynth -> "Pad3Polysynth"
    Pad4Choir -> "Pad4Choir"
    Pad5Bowed -> "Pad5Bowed"
    Pad6Metallic -> "Pad6Metallic"
    Pad7Halo -> "Pad7Halo"
    Pad8Sweep -> "Pad8Sweep"
    PanFlute -> "PanFlute"
    PercussiveOrgan -> "PercussiveOrgan"
    Piccolo -> "Piccolo"
    PizzicatoStrings -> "PizzicatoStrings"
    Recorder -> "Recorder"
    ReedOrgan -> "ReedOrgan"
    ReverseCymbal -> "ReverseCymbal"
    RockOrgan -> "RockOrgan"
    Seashore -> "Seashore"
    Shakuhachi -> "Shakuhachi"
    Shamisen -> "Shamisen"
    Shanai -> "Shanai"
    Sitar -> "Sitar"
    SlapBass1 -> "SlapBass1"
    SlapBass2 -> "SlapBass2"
    SopranoSax -> "SopranoSax"
    SteelDrums -> "SteelDrums"
    StringEnsemble1 -> "StringEnsemble1"
    StringEnsemble2 -> "StringEnsemble2"
    SynthBass1 -> "SynthBass1"
    SynthBass2 -> "SynthBass2"
    SynthBrass1 -> "SynthBrass1"
    SynthBrass2 -> "SynthBrass2"
    SynthChoir -> "SynthChoir"
    SynthDrum -> "SynthDrum"
    SynthStrings1 -> "SynthStrings1"
    SynthStrings2 -> "SynthStrings2"
    TaikoDrum -> "TaikoDrum"
    TangoAccordion -> "TangoAccordion"
    TelephoneRing -> "TelephoneRing"
    TenorSax -> "TenorSax"
    Timpani -> "Timpani"
    TinkleBell -> "TinkleBell"
    TremoloStrings -> "TremoloStrings"
    Trombone -> "Trombone"
    Trumpet -> "Trumpet"
    Tuba -> "Tuba"
    TubularBells -> "TubularBells"
    Vibraphone -> "Vibraphone"
    Viola -> "Viola"
    Violin -> "Violin"
    VoiceOohs -> "VoiceOohs"
    Whistle -> "Whistle"
    Woodblock -> "Woodblock"
    Xylophone -> "Xylophone"

-- | the set of supported instruments
instrumentNames :: List InstrumentName
instrumentNames =
  map snd mapping

-- | the set of supported instruments, using the Gleitzman names
gleitzmanNames :: List String
gleitzmanNames =
  map (toGleitzmanName <<< fst) mapping

-- | read a Gleitzman instrument name amd attempt to convert to a MIDI instrument.
readGleitzman :: String -> Maybe InstrumentName
readGleitzman g =
  lookup g gleitzmanNamesMap


gleitzmanNamesMap :: Map String InstrumentName
gleitzmanNamesMap =
  let
    f :: Tuple String InstrumentName -> Tuple String InstrumentName
    f (Tuple s i) = Tuple (toGleitzmanName s) i
  in
    fromFoldable $ map f mapping

mapping :: List (Tuple String InstrumentName)
mapping =
     Tuple "Accordion" Accordion
   : Tuple "AcousticBass" AcousticBass
   : Tuple "AcousticGrandPiano" AcousticGrandPiano
   : Tuple "AcousticGuitarNylon" AcousticGuitarNylon
   : Tuple "AcousticGuitarSteel" AcousticGuitarSteel
   : Tuple "Agogo" Agogo
   : Tuple "AltoSax" AltoSax
   : Tuple "Applause" Applause
   : Tuple "Bagpipe" Bagpipe
   : Tuple "Banjo" Banjo
   : Tuple "BaritoneSax" BaritoneSax
   : Tuple "Bassoon" Bassoon
   : Tuple "BirdTweet" BirdTweet
   : Tuple "BlownBottle" BlownBottle
   : Tuple "BrassSection" BrassSection
   : Tuple "BreathNoise" BreathNoise
   : Tuple "BrightAcousticPiano" BrightAcousticPiano
   : Tuple "Celesta" Celesta
   : Tuple "Cello" Cello
   : Tuple "ChoirAahs" ChoirAahs
   : Tuple "ChurchOrgan" ChurchOrgan
   : Tuple "Clarinet" Clarinet
   : Tuple "Clavinet" Clavinet
   : Tuple "Contrabass" Contrabass
   : Tuple "DistortionGuitar" DistortionGuitar
   : Tuple "DrawbarOrgan" DrawbarOrgan
   : Tuple "Dulcimer" Dulcimer
   : Tuple "ElectricBassFinger" ElectricBassFinger
   : Tuple "ElectricBassPick" ElectricBassPick
   : Tuple "ElectricGrandPiano" ElectricGrandPiano
   : Tuple "ElectricGuitarClean" ElectricGuitarClean
   : Tuple "ElectricGuitarJazz" ElectricGuitarJazz
   : Tuple "ElectricGuitarMuted" ElectricGuitarMuted
   : Tuple "ElectricPiano1" ElectricPiano1
   : Tuple "ElectricPiano2" ElectricPiano2
   : Tuple "EnglishHorn" EnglishHorn
   : Tuple "Fiddle" Fiddle
   : Tuple "Flute" Flute
   : Tuple "FrenchHorn" FrenchHorn
   : Tuple "FretlessBass" FretlessBass
   : Tuple "Fx1Rain" Fx1Rain
   : Tuple "Fx2Soundtrack" Fx2Soundtrack
   : Tuple "Fx3Crystal" Fx3Crystal
   : Tuple "Fx4Atmosphere" Fx4Atmosphere
   : Tuple "Fx5Brightness" Fx5Brightness
   : Tuple "Fx6Goblins" Fx6Goblins
   : Tuple "Fx7Echoes" Fx7Echoes
   : Tuple "Fx8Scifi" Fx8Scifi
   : Tuple "Glockenspiel" Glockenspiel
   : Tuple "GuitarFretNoise" GuitarFretNoise
   : Tuple "GuitarHarmonics" GuitarHarmonics
   : Tuple "Gunshot" Gunshot
   : Tuple "Harmonica" Harmonica
   : Tuple "Harpsichord" Harpsichord
   : Tuple "Helicopter" Helicopter
   : Tuple "HonkytonkPiano" HonkytonkPiano
   : Tuple "Kalimba" Kalimba
   : Tuple "Koto" Koto
   : Tuple "Lead1Square" Lead1Square
   : Tuple "Lead2Sawtooth" Lead2Sawtooth
   : Tuple "Lead3Calliope" Lead3Calliope
   : Tuple "Lead4Chiff" Lead4Chiff
   : Tuple "Lead5Charang" Lead5Charang
   : Tuple "Lead6Voice" Lead6Voice
   : Tuple "Lead7Fifths" Lead7Fifths
   : Tuple "Lead8BassLead" Lead8BassLead
   : Tuple "Marimba" Marimba
   : Tuple "MelodicTom" MelodicTom
   : Tuple "MusicBox" MusicBox
   : Tuple "MutedTrumpet" MutedTrumpet
   : Tuple "Oboe" Oboe
   : Tuple "Ocarina" Ocarina
   : Tuple "OrchestraHit" OrchestraHit
   : Tuple "OrchestralHarp" OrchestralHarp
   : Tuple "OverdrivenGuitar" OverdrivenGuitar
   : Tuple "Pad1NewAge" Pad1NewAge
   : Tuple "Pad2Warm" Pad2Warm
   : Tuple "Pad3Polysynth" Pad3Polysynth
   : Tuple "Pad4Choir" Pad4Choir
   : Tuple "Pad5Bowed" Pad5Bowed
   : Tuple "Pad6Metallic" Pad6Metallic
   : Tuple "Pad7Halo" Pad7Halo
   : Tuple "Pad8Sweep" Pad8Sweep
   : Tuple "PanFlute" PanFlute
   : Tuple "PercussiveOrgan" PercussiveOrgan
   : Tuple "Piccolo" Piccolo
   : Tuple "PizzicatoStrings" PizzicatoStrings
   : Tuple "Recorder" Recorder
   : Tuple "ReedOrgan" ReedOrgan
   : Tuple "ReverseCymbal" ReverseCymbal
   : Tuple "RockOrgan" RockOrgan
   : Tuple "Seashore" Seashore
   : Tuple "Shakuhachi" Shakuhachi
   : Tuple "Shamisen" Shamisen
   : Tuple "Shanai" Shanai
   : Tuple "Sitar" Sitar
   : Tuple "SlapBass1" SlapBass1
   : Tuple "SlapBass2" SlapBass2
   : Tuple "SopranoSax" SopranoSax
   : Tuple "SteelDrums" SteelDrums
   : Tuple "StringEnsemble1" StringEnsemble1
   : Tuple "StringEnsemble2" StringEnsemble2
   : Tuple "SynthBass1" SynthBass1
   : Tuple "SynthBass2" SynthBass2
   : Tuple "SynthBrass1" SynthBrass1
   : Tuple "SynthBrass2" SynthBrass2
   : Tuple "SynthChoir" SynthChoir
   : Tuple "SynthDrum" SynthDrum
   : Tuple "SynthStrings1" SynthStrings1
   : Tuple "SynthStrings2" SynthStrings2
   : Tuple "TaikoDrum" TaikoDrum
   : Tuple "TangoAccordion" TangoAccordion
   : Tuple "TelephoneRing" TelephoneRing
   : Tuple "TenorSax" TenorSax
   : Tuple "Timpani" Timpani
   : Tuple "TinkleBell" TinkleBell
   : Tuple "TremoloStrings" TremoloStrings
   : Tuple "Trombone" Trombone
   : Tuple "Trumpet" Trumpet
   : Tuple "Tuba" Tuba
   : Tuple "TubularBells" TubularBells
   : Tuple "Vibraphone" Vibraphone
   : Tuple "Viola" Viola
   : Tuple "Violin" Violin
   : Tuple "VoiceOohs" VoiceOohs
   : Tuple "Whistle" Whistle
   : Tuple "Woodblock" Woodblock
   : Tuple "Xylophone" Xylophone
   : Nil

-- | Convert a 'show' name to a Gleitzman name
-- | for example ElectricPiano1 -> electric_piano_1
toGleitzmanName :: String -> String
toGleitzmanName inst =
  -- the leading capital induces an unwanted underscore which we must drop
  fromCharArray $ drop 1 $ foldr f [] (toCharArray inst)
    where
      f :: Char -> Array Char -> Array Char
      f c acc =
        -- all capitals should invoke an underscore unless at the start
        if (isUpper c) then
          cons '_' (cons (toLower c) acc)
        else if (isDigit c) then
          cons '_' (cons c acc)
        else
          cons (toLower c) acc

-- the Gleitzman file names are all ASCII.  We therefore use ASCII only
-- versions of isUpper, isDigit and toLower
-- this seems to save us about 300K of generated code!

asciiUpper :: Array Char
asciiUpper =
  ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O',
   'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z']

asciiLower :: Array Char
asciiLower =
  ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o',
   'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z']

asciiDigit :: Array Char
asciiDigit =
  ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']

isUpper :: Char -> Boolean
isUpper c =
  contains asciiUpper c

isDigit :: Char -> Boolean
isDigit c =
  contains asciiDigit c

toLower :: Char -> Char
toLower c =
  case (elemIndex c asciiUpper) of
    Just ix ->
      fromMaybe c $ index asciiLower ix
    _ -> c

contains :: ∀ a. Eq a => Array a -> a -> Boolean
contains xs x =
  (length $ filter (\y -> y == x) xs) > 0
