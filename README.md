purescript-midi
===============

This library provides MIDI support in PureScript.  It provides a MIDI parser and also support for the Web MIDI API (currently only available in Chrome and, I think, Opera). For example, if you use Chrome and can plug in a MIDI keyboard you should to be able to play using [this](http://www.tradtunedb.org.uk:8600/).

## Building

    $ bower install   
    $ pulp build

## Midi Parser

Data.Midi.Parser is a parser for MIDI that uses [purescript-string-parsers](https://pursuit.purescript.org/packages/purescript-string-parsers/2.1.0) (i.e. it is not a wrapper for MIDI.js).  It is to a large extent a port of the [Elm version](http://package.elm-lang.org/packages/newlandsvalley/elm-comidi/latest) and for this reason it uses the string parser combinator library and not [purescript-parsing](https://pursuit.purescript.org/packages/purescript-parsing/3.0.0) which I have not yet had the opportunity to investigate. It expects as input a binary MIDI file 'tunnelled' as text (such as can be achieved by means of _overideMimeType_ in XMLHttpRequests). The _normalise_ function is used to make sense of this pseudo-string.

To parse a MIDI string that represents a recording and thus generate a Midi.Recording you can use:

    (parse <<< normalise) midi

or, if the MIDI uses running status messages:

    (translateRunningStatus <<< parse <<< normalise) midi

so that these messages are translated to the underlying channel messages.

On the other hand, you may merely need to parse MIDI events (such as note on or note off). This is more likely if you are connecting directly to a MIDI device and need to parse the stream of event messages as the instrument is played.  To do this, use:

    parseMidiEvent midiEvent

This version is intended to be a fully conformant parser which is happy with Type-0, Type-1 and Type-2 files.

## Web MIDI Support

Data.Midi.WebMidi provides support for [web MIDI](https://www.w3.org/TR/webmidi/).  If you have a browser that supports this API, you can connect a MIDI input device and listen to MIDI messages that are produced (for example) whenever a key is pressed.

## Examples

* __midifile__ demonstrates basic MIDI file parsing.
* __abcfile__ demonstrates converting an ABC file to a MIDI recording.
* __keyboard__ demonstrates a playable keyboard that uses a variety of instrument soundfonts.

and three different players: 

* __simpleplayer__ demonstrates playing a MIDI file by converting to a single uninterruptible Web-Audio graph.
* __player__ demonstrates a MIDI player widget that plays each note as an individual Web-Audio instruction.
* __hybridplayer__ demonstrates a MIDI player widget that plays a phrase (consisting of a bunch of notes) before allowing a UI update.
