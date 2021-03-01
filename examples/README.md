purescript-midi examples
========================

All purescript-midi examples use purescript-pux as the web framework although most of the external projects (other than MIDI Keyboard) use Halogen. 

### v2.0.0 (PureScript 0.12)

* __keyboard-signal__ log web-midi messages.

see also: 
 [MIDI Keyboard](https://github.com/newlandsvalley/purescript-midi-keyboard),
 [ABC Editor](https://github.com/newlandsvalley/purescript-abc-editor),
 [PureScript School of Music Editor](https://github.com/newlandsvalley/purescript-school-of-music/tree/master/editor) etc.

### v1.2.0 (deprecated - PureScript 0.11)

* __midifile__ load MIDI from the local file system and then parse it.
* __midiurl__ loading MIDI from a URL and then parse it.
* __simpleplayer__ play a MIDI file by converting to a single uninterruptible Web-Audio graph.

### v1.0.0 (deprecated) 

These are now significantly out of date because they use async effects (Aff) for web-midi rather than signals. Soundfonts are taken from polyphonic-soundfonts 1.0.0.

* __abcfile__ convert an ABC file to a MIDI recording.
* __keyboard__  a playable keyboard that uses a variety of instrument soundfonts.

and two other Pux-based players:

* __player__ a MIDI player widget that plays each note as an individual Web-Audio instruction.
* __hybridplayer__ a MIDI player widget that plays a phrase (consisting of a bunch of notes) before allowing a UI update.

