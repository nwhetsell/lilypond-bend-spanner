\version "2.19.46"

bendHold =
#(define-event-function (mus)(ly:music?)
#{
  \tweak style #'hold
  $mus
#})

preBend =
#(define-event-function (mus)(ly:music?)
#{
  \tweak style #'pre-bend
  \tweak details.head-text-break-visibility ##(#t #t #f)
  $mus
#})

preBendHold =
#(define-event-function (mus)(ly:music?)
#{
  \tweak style #'pre-bend-hold
  \tweak details.head-text-break-visibility ##(#t #t #f)
  $mus
#})

bendStencilTweak =
#(define-event-function (integer music)(integer? ly:music?)
#{
  \tweak stencil #(bend::draw-bend-arrow-stencil integer) $music
#})

