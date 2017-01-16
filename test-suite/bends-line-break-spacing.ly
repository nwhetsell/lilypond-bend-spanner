\version "2.19.46"

%#(format #t "\ncompiling ----------- ~a in ~a.ly"
%  (car (ly:input-file-line-char-column (*location*)))
%  (ly:parser-output-name))

%% How to restrict clef-changes to Staff?
%% Anyway, it's done only for spacing tests

mus-line-break-spacing = {
  %% clef
  a'1\startBend \break \clef "alto" b'\stopBend
  %% time-sig
  e'\startBend \break \time 2/2 fis'\stopBend
  %% key-sig
  c'\startBend \break \key b \major d'\stopBend
  %% key-cancellation/key-sig
  g\startBend \break \key des \major a\stopBend
  %% all together
  d\startBend \break \clef "treble" \time 8/8 \key cis \major e\stopBend
  \bar "|."
}

\bookpart {
  \score {
    \new StaffGroup
      <<
        \new Staff { \clef "G_8" \mus-line-break-spacing }
        \new TabVoice \mus-line-break-spacing
    >>
    \header {
      piece =
        \markup \rounded-box \fill-line {
          \wordwrap-string
            #"Testing spacing at line-break.

At line-break the BendSpanner should stop before changes for time, key, clef in
the Staff are printed."
            \null
        }
    }
    \layout {
      \context {
        \TabStaff
          minimumFret = #3
          restrainOpenStrings = ##t
      }
    }
  }
}
