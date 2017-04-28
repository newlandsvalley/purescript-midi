Simple MIDI player example
==========================

This example shows how you might load and then play a MIDI file as a single
uninterrupted Web-Audio melody. Each MIDI NoteOn event produces a MidiNote
(an instruction to Web-Audio to play through the current SoundFont). This
has an offset which is the time delay after which the note should be played
and is built simply by accumulating the elapsed time of each MIDI message.
Each note rings for a second.

Building
--------

From the current directory

    $ bower install
    $ ./build.sh

The code is built as example.js in the simpleplayer/dist directory. This is runnable if you open the corresponding index.html.
