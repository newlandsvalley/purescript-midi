purescript-midi
===============

[![Latest release](http://img.shields.io/github/release/newlandsvalley/purescript-midi.svg)](https://github.com/newlandsvalley/purescript-midi/releases)
[![Build Status](https://travis-ci.org/newlandsvalley/purescript-midi.svg?branch=master)](https://travis-ci.org/newlandsvalley/purescript-midi)

MIDI support in the browser.

This library allows you to both parse and generate MIDI event streams and MIDI recordings. It also allows you to connect to a MIDI instrument (such as a keyboard) via the Web MIDI API (currently only available in Chrome and, I think, Opera). For example, if you use Chrome and can plug in a MIDI keyboard you should to be able to play it using [this](http://www.tradtunedb.org.uk:8601/) demonstration.

To parse a MIDI string that represents a recording and thus generate a value of type __Midi.Recording__ you can use:

    (parse <<< normalise) midiString

On the other hand, you may merely need to parse MIDI events (such as note on or note off) that emenate from a Web MIDI connection. In other words, you are connecting directly to a MIDI device through the browser and need to parse the stream of event messages as the instrument is played.  To do this, use:

    parseMidiEvent midiEvent

This will attempt to parse an individual event.    

## Installation

    bower install purescript-midi

## Examples

See examples.
