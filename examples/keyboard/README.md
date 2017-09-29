Web-Midi example - MIDI Keyboard
================================

This example shows how you can detect MIDI input devices such as keyboards when they
connect or disconnect and then detect MIDI messages when (for example) keys are pressed.
You will need a MIDI keyboard or other MIDI device to try this module. 

Every message from the device is detected, but the application responds only to Note On, Note Off and Volume Change messages, by playing each note through the selected soundfont.  This could be extended in order to build a full MIDI synthesizer. Note that Web-Midi is only currently supported in Chrome. 

You must first connect to Web-Midi - if this is successful then an acoustic grand piano soundfont is loaded. You can change this for any instrument selected from [Benjamin Gleitzman's soundfont library](https://github.com/gleitz/midi-js-soundfonts).

Building
--------

From the current directory

    $ bower install
    $ ./build.sh

The code is built as example.js in the keyboard/dist directory. This is runnable if you open the corresponding index.html.
