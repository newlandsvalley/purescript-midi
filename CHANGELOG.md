# Changelog

### Version 4.0,0 (2022-04-06)

* PS Compiler 0.15.0
* (replace test-unit with spec)

### Version 3.1.0 (2022-03-21)

* PS Compiler 0.14.7
* Enforced upgrade because of breaking changes in string-parsers 7.0.0

### Version 3.0.0 (2021-03-02)

* PS Compiler 0.14.0
* Deprecate examples which only exist for old versions of the PS compiler

### Version 2.3.0 (2020-11-18)

* Add spago

### Version 2.2.0 (2019-05-13)

* Reduce code bloat in Midi Instruments

### Version 2.0.0

* PS Compiler 0.12

### Version 1.2.0 (2018-02-02)

#### breaking changes

* Expand running status messages in situ.  
* Get rid of translateRunningStatus post-processing
* Disallow running status in event streams
* Separate stream event handling from file event handling
  (SysEx parsing is different in the two contexts)
* Instrument.instrumentNames now returns a list of InstrumentName.  The previous
  version returning a list of strings is renamed to gleitzmanNames.

#### additions
* Add 'binary' MIDI generation
* Add type aliases for Channel, Velocity, Byte etc.

#### bug fixes
* Correct varInt parsing for large integer values

### Version 1.1.0  (2017-11-04)

* Web-Midi messages re-implemented in terms of Signal.

### Version 1.0.0  (2017-10-16)

* Initial release.
