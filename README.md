purescript-midi
===============

[![Latest release](http://img.shields.io/github/release/newlandsvalley/purescript-midi.svg)](https://github.com/newlandsvalley/purescript-midi/releases)
[![Build Status](https://travis-ci.org/newlandsvalley/purescript-midi.svg?branch=master)](https://travis-ci.org/newlandsvalley/purescript-midi)

MIDI support in the browser.

This library allows you to both parse and generate MIDI event streams and MIDI recordings. It also allows you to connect to a MIDI instrument (such as a keyboard) via the Web MIDI API. For example, if you use Chrome and can plug in a MIDI keyboard you should to be able to play it using [this](http://www.tradtunedb.org.uk:8601/) demonstration.

To parse a MIDI string that represents a recording and thus generate a value of type __Midi.Recording__ you can use:

    (parse <<< normalise) midiString

On the other hand, you may merely need to parse MIDI events (such as note on or note off) that emanate from a Web MIDI connection. In other words, you are connecting directly to a MIDI device through the browser and need to parse the stream of event messages as the instrument is played.  To do this, use:

    parseMidiEvent midiEvent

This will attempt to parse an individual event.    

## To Build

    npm run build 

## To Test

    npm run test

## Examples

There are examples in the _examples_ folder, however nearly all of these were developed for earlier releases of PureScript (many using Pux) and will be removed at some stage. Nevertheless they give some idea of how the library may be used.

## Web MIDI Permissions
It appears that browsers are becoming stricter about allowing Web MIDI to run from 'untrusted' servers after being notified of security exploits.  At the time of writing, both Chrome (see [details](https://www.chromestatus.com/feature/5138066234671104)) and Mozilla require HTTPS connections. 

## Module documentation

Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-midi).
