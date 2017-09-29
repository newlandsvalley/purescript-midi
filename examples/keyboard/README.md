Web-Midi example - MIDI Keyboard
================================

Try it [here](http://www.tradtunedb.org.uk:8601/).

This example shows how you can detect MIDI input devices such as keyboards when they
connect or disconnect and then detect MIDI messages when (for example) keys are pressed.
You will need a MIDI keyboard or other such device to try it. 

Every message from the device is detected, but the application responds only to Note On and Volume Change messages by playing each note (at the required volume) through the selected soundfont for a preset duration.  This illustrates how you might start to build a MIDI synthesizer. Note that at the time of writing, Web-Midi is only supported in Chrome and Opera. 

You must first connect to Web-Midi - if this is successful then an acoustic grand piano soundfont is loaded. You can change this for any instrument selected from [Benjamin Gleitzman's soundfont library](https://github.com/gleitz/midi-js-soundfonts).

Building
--------

From the current directory

    $ bower install
    $ ./build.sh

The code is built as example.js in the keyboard/dist directory. This is runnable if you open the corresponding index.html.
