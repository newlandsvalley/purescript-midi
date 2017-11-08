purescript-midi
===============

[![Latest release](http://img.shields.io/github/release/newlandsvalley/purescript-midi.svg)](https://github.com/newlandsvalley/purescript-midi/releases)
[![Build Status](https://travis-ci.org/newlandsvalley/purescript-midi.svg?branch=master)](https://travis-ci.org/newlandsvalley/purescript-midi)

This library provides MIDI support in PureScript.  It includes a MIDI parser and also has support for the Web MIDI API (currently only available in Chrome and, I think, Opera). For example, if you use Chrome and can plug in a MIDI keyboard you should to be able to play it using [this](http://www.tradtunedb.org.uk:8601/) demonstration.

## Building

    $ bower install   
    $ pulp build

## Midi Parser

Data.Midi.Parser is a parser for general MIDI with no native dependencies (i.e. it is not a wrapper for MIDI.js). It is a fully conformant parser which is happy to receive Type-0, Type-1 and Type-2 MIDI input. It expects binary MIDI 'tunnelled' as text (such as can be achieved by means of _overideMimeType_ in XMLHttpRequests). The _normalise_ function is used to make sense of this pseudo-string.

To parse a MIDI string that represents a recording and thus generate a value of type __Midi.Recording__ you can use:

    (parse <<< normalise) midi

or, if the MIDI uses running status messages:

    (translateRunningStatus <<< parse <<< normalise) midi

so that these messages are translated to the underlying channel messages.

On the other hand, you may merely need to parse MIDI events (such as note on or note off). This is more likely if you are connecting directly to a MIDI device and need to parse the stream of event messages as the instrument is played.  To do this, use:

    parseMidiEvent midiEvent

## Web MIDI

Data.Midi.WebMidi provides access to the [web MIDI](https://www.w3.org/TR/webmidi/) platform API and hence is only available within browsers that support that API. You can connect a MIDI input device and listen to MIDI messages that are produced (for example) whenever a key is pressed. Currently there is no support for MIDI output devices.

## Midi Instrument

Data.Midi.Instrument is an enumerated type listing the various musical instruments supported by general MIDI. It includes a two-way translation between it and the instrument names used in Benjamin Gleitzman's [SoundFont](http://gleitz.github.io/midi-js-soundfonts/FluidR3_GM/names.json) library.

