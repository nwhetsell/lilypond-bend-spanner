\version "2.19.46"

%#(format #t "\ncompiling ----------- ~a in ~a.ly"
%  (car (ly:input-file-line-char-column (*location*)))
%  (ly:parser-output-name))

mus-full-notation = {
  a,4.\6 b,8 a,4.\6\startBend b,8\6\stopBend
  a,4..\6 b,16 a,4..\6\startBend b,16\6\stopBend
  a,4...\6 b,32 a,4...\6\startBend b,32\6\stopBend

  a,8.\6 b,16 a,8.\6\startBend b,16\6\stopBend
  a,8.[\6\startBend b,8.]\6\stopBend

  \bar "|."
}

\bookpart {
  \score {
    <<
      \new Staff { \clef "G_8" \mus-full-notation }
      \new TabVoice {
        <>_"default target-visibility, i.e. #f" \mus-full-notation
      }
      \new TabVoice
        \with { \override BendSpanner.details.target-visibility = ##t }
        { <>_"target-visibility set #t" \mus-full-notation }
    >>
    \header {
      piece =
        \markup \rounded-box \fill-line {
            \wordwrap-string
              #"Testing bend with \\tabFullNotation.

Collision with dots is avoided, if spacing is not too tight. Currently this
happens more by accident.

TODO take the dots-extension into account?

Bends target is made transparent relying on details.target-visibility, default
setting is #f.

Including TabNoteHead, Stem, Flag, Dots.
The Beam is present.

TODO needs design-decision: should Beam be transparent as well?

If yes, a different approach from `make-tab-heads-transparent' is needed."
              \null
        }
    }
    \layout {
      \context {
        \TabStaff
        minimumFret = #3
        restrainOpenStrings = ##t
        \tabFullNotation
      }
    }
  }
}
