Web-Midi example
================

This example shows how you can detect MIDI input devices such as keyboards when they
connect or disconnect and then detect MIDI messages when (for example) keys are pressed.
You will need a MIDI keyboard or other MIDI device to try this module.
The MIDI messages are interpreted - which responds to NoteOn messages (played through an instrument soundfont) and Volume Control messages. Note that Web-Midi is only currently supported in Chrome. 

Building
--------

From the current directory

    $ bower install
    $ ./build.sh

The code is built as example.js in the keyboard/dist directory. This is runnable if you open the corresponding index.html.
