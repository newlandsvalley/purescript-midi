Hybrid MIDI player example
==========================

purescript-midi version 1.0.0

This example is a MIDI player that is a hybrid between the 'player' and the 'simpleplayer' examples.  The simpleplayer
plays strictly in tempo but is uninterruptible. The widget in the player example allows the user to stop and start the playback but,
because the UI is re-rendered after every note, fails to play strictly in time.  This hybrid version splits the MIDI performance
into phrases of roughly the same duration and allows interruption only after the current phrase has finished playing. It also
defines an exact duration for each note by measuring the difference in time between each NoteOn event and its accompanying
NoteOff. You can alter the phrase length in order to attempt to get a good balance between responsiveness of the widget buttons
and playing the tune at the appropriate tempo.

This is an experiment from which the [purescript-midi-player](https://github.com/newlandsvalley/purescript-midi-player) has been derived.

Building
--------

From the current directory

    $ bower install
    $ ./build.sh

The code is built as example.js in the hybridplayer/dist directory. This is runnable if you open the corresponding index.html.
