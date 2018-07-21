Web-Midi example - Keyboard-Signal
==================================

purescript-midi version 2.0.0 (PureScript 12.0)

This is a basic example that shows how you can detect MIDI input devices such as keyboards when they
connect or disconnect and then detect MIDI messages when (for example) keys are pressed.
You will need a MIDI keyboard or other such device to try it.

You must first connect to Web-Midi - then you subscribe to signals of input device connection/disconnection messages and to Midi Event messages that occur when keys are pressed.  These events are then logged to the console.

Building
--------

From the current directory

    $ bower install
    $ ./build.sh

The code is built as example.js in the keyboard/dist directory. This is runnable if you open the corresponding index.html.
