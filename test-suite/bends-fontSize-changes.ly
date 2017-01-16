\version "2.19.46"

%#(format #t "\ncompiling ----------- ~a in ~a.ly"
%  (car (ly:input-file-line-char-column (*location*)))
%  (ly:parser-output-name))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% testing changes for fontSize and thickness
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% short-cut from LSR
staffSize = #(define-music-function (parser location new-size) (number?)
  #{
    \set fontSize = #new-size
    \override StaffSymbol.staff-space = #(magstep new-size)
    \override StaffSymbol.thickness = #(magstep new-size)
  #})

mus-fontSize-change = {
    a,4\6 \startBend b,\6\stopBend
}

\bookpart {
  \score {
    <<
      \new Staff { \clef "G_8" \mus-fontSize-change }
      \new TabStaff \with { \staffSize #-12 } \mus-fontSize-change
      \new TabStaff \mus-fontSize-change
      \new TabStaff \with { \staffSize #12 } \mus-fontSize-change
    >>
    \header {
      piece =
        \markup \rounded-box \fill-line {
          \wordwrap-string
            #"Testing changes for fontSize, StaffSymbol.thickness and
StaffSymbol.staff-space"
            \null
        }
    }
  }
}