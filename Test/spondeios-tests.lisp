(in-package #:spondeios-tests)
(in-suite :spondeios)

(test spondeios-initialize-context-manually
      (init :spheres (list 'spondeios:flagging-sphere 'spondeios:networking-sphere 'spondeios:verbose-sphere) :space 'spondeios:verbose-memory-space))