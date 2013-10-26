(in-package #:spondeios-tests)
(in-suite :spondeios)

(test spondeios-initialize-context-manually-with-symbols
  (init :spheres (list 'spondeios:flagging-sphere 'spondeios:networking-sphere 'spondeios:verbose-sphere) :space 'spondeios:verbose-memory-space))

(test spondeios-initialize-context-manually-with-object
  (init :space (make-instance 'spondeios:verbose-memory-space)))