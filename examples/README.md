purescript-midi examples
========================

### v1.1.0 (or newer) examples

* __keyboard-signal__ demonstrates logging web-midi messages.
* __midifile__ demonstrates basic MIDI file parsing.
* __simpleplayer__ demonstrates playing a MIDI file by converting to a single uninterruptible Web-Audio graph.

### v1.0.0 examples

These are now a little out of date because they use async effects (Aff) for web-midi rather than signals. Soundfonts are taken from polyphonic-soundfonts 1.0.0.

* __abcfile__ demonstrates converting an ABC file to a MIDI recording.
* __keyboard__ demonstrates a playable keyboard that uses a variety of instrument soundfonts.

and two other players: 

* __player__ demonstrates a MIDI player widget that plays each note as an individual Web-Audio instruction.
* __hybridplayer__ demonstrates a MIDI player widget that plays a phrase (consisting of a bunch of notes) before allowing a UI update.

For further up-to-date examples, see the following projects:  [MIDI Keybioard](https://github.com/newlandsvalley/purescript-midi-keyboard), [ABC Editor](https://github.com/newlandsvalley/purescript-abc-editor), [PureScript School of Music Editor](https://github.com/newlandsvalley/purescript-school-of-music/tree/master/editor) etc.
