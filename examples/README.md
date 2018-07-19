purescript-midi examples
========================

All examples use purescript-pux as the web framework.  At the time of writing, Pux had not been upgraded to PureScript 0.12 and so the examples remain at PureScript 0.11.

### v1.2.0 examples

* __keyboard-signal__ log web-midi messages.
* __midifile__ load MIDI from the local file system and then parse it.
* __midiurl__ loading MIDI from a URL and then parse it.
* __simpleplayer__ play a MIDI file by converting to a single uninterruptible Web-Audio graph.

### v1.0.0 (deprecated) examples

These are now a little out of date because they use async effects (Aff) for web-midi rather than signals. Soundfonts are taken from polyphonic-soundfonts 1.0.0.

* __abcfile__ convert an ABC file to a MIDI recording.
* __keyboard__  a playable keyboard that uses a variety of instrument soundfonts.

and two other players:

* __player__ a MIDI player widget that plays each note as an individual Web-Audio instruction.
* __hybridplayer__ a MIDI player widget that plays a phrase (consisting of a bunch of notes) before allowing a UI update.

For further up-to-date examples, see the following projects:
 [MIDI Keybioard](https://github.com/newlandsvalley/purescript-midi-keyboard),
 [ABC Editor](https://github.com/newlandsvalley/purescript-abc-editor),
 [PureScript School of Music Editor](https://github.com/newlandsvalley/purescript-school-of-music/tree/master/editor) etc.
