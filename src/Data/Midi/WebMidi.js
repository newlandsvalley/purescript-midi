"use strict";

var wrapper = function() {

  var messageCallback;
  var connectionCallback;

  function onMIDISuccessListen (midiAccess) {

    // console.log('onMIDISuccessListen');

    var inputs = midiAccess.inputs.values();
    // loop over any register inputs and listen for data on each
    midiAccess.inputs.forEach( function( input, id, inputMap ) {
      input.onmidimessage = onMIDIMessage;
    });
  };

  function onMIDISuccessDetect (midiAccess) {

    // console.log('onMIDISuccessDetect');

    var inputs = midiAccess.inputs.values();
    // loop over any register inputs and listen for data on each
    midiAccess.inputs.forEach( function( input, id, inputMap ) {
      registerInput(input);
    });
    // listen for connect/disconnect message
    midiAccess.onstatechange = onStateChange;
  };

  // register an input device
  function registerInput (input) {
     /*
     console.log("Input port : [ type:'" + input.type + "' id: '" + input.id +
        "' manufacturer: '" + input.manufacturer + "' name: '" + input.name +
        "' version: '" + input.version + "']");
     */
     var midiConnection = { connected : true
                          , portType : input.type
                          , id : input.id
                          , manufacturer : input.manufacturer
                          , name : input.name
                          , version : input.version };

     connectionCallback(midiConnection)();
  };

  // input connect/disconnect signal
  function onStateChange (event) {
	  // showMIDIPorts(midi);
	  var port = event.port, state = port.state, name = port.name, type = port.type, id = port.id;
	  if (port.type == "input") {
      // console.log("State change:", state);
      if (state == "connected") {
        var midiConnection = {  connected : true
                              , portType : port.type
                              , id : port.id
                              , manufacturer : port.manufacturer
                              , name : port.name
                              , version : port.version };

        port.onmidimessage = onMIDIMessage;
        connectionCallback(midiConnection)();
      }
      else if  (state == "disconnected") {
        var midiDisconnection = {  connected : false
                                 , portType : port.type
                                 , id : port.id
                                 , manufacturer : port.manufacturer
                                 , name : port.name
                                 , version : port.version };

        connectionCallback(midiDisconnection)();
        }
    }
}

  function onMIDIMessage (event) {
    // sourceId = event.srcElement.id;
    // console.log("MIDI Message");
    var encodedEvent = { timeStamp : event.timeStamp
                       , encodedBinary : encodeAsString(event.data)};
    messageCallback(encodedEvent)();
  };

  function encodeAsString (data) {
    var dataLength = data.length;
    var encoded = "";
    for (var i = 0; i < dataLength; i++) {
      encoded += String.fromCharCode(data[i]);
    }
    return encoded;
  };

  return {

    webMidiConnect: function () {
      return (navigator.requestMIDIAccess);
    },

    listenImpl: function (midiMessageCallback) {

      messageCallback = midiMessageCallback;

      return function () {
        // console.log('MIDIConnect - listen');
        // request MIDI access and then connect
        if (navigator.requestMIDIAccess) {
          navigator.requestMIDIAccess({
            sysex: false // this defaults to 'false' anyway.
          }).then(onMIDISuccessListen)
        }
      }
    },

    detectInputDevicesImpl: function (connectCallback) {

      connectionCallback = connectCallback;

      return function () {
        console.log('MIDIConnect - detect');
        // request MIDI access and then connect
        if (navigator.requestMIDIAccess) {
          navigator.requestMIDIAccess({
            sysex: false // this defaults to 'false' anyway.
          }).then(onMIDISuccessDetect)
        }
      }
    }
  }
}();

export var webMidiConnect = wrapper.webMidiConnect;
export var listen = wrapper.listenImpl;
export var detectInputDevices = wrapper.detectInputDevicesImpl;
