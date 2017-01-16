\version "2.19.46"

%#(format #t "\ncompiling ----------- ~a in ~a.ly"
%  (car (ly:input-file-line-char-column (*location*)))
%  (ly:parser-output-name))

horizontal-spacing-at-start = {
  a,4\6\startBend b,4\6\stopBend
  a4\6\startBend b4\6\stopBend
  a''''''''4\6\startBend b''''''''4\6\stopBend
  a,4\6\startBend b,4\6\stopBend

  <f' a'>4\startBend <g' b'>4\stopBend
  <c' e'>4\startBend <d' fis'>4\stopBend
  <a c'>4\startBend <b d'>4\stopBend
  <e g>4\startBend <fis a>4\stopBend
  <b, d>4\startBend <c e>4\stopBend
  <b, d>4\startBend <c e>4\stopBend

  <a'\2 cis''\1>4\startBend <bes' d''>4\stopBend
  \bar "|."
}

\bookpart {
  \paper { indent = 50 }
  \score {
    <<
      \new Staff { \clef "G_8" \horizontal-spacing-at-start }
      \new TabStaff
        \with { instrumentName = "default spacing" }
        \horizontal-spacing-at-start
      \new TabStaff
        \with {
          instrumentName =
            \markup \center-column {
                "details.horizontal-left-padding" "changed"
            }
          \override BendSpanner.details.horizontal-left-padding = #0.4
        }
        \horizontal-spacing-at-start
    >>
    \header {
      piece =
        \markup \rounded-box \fill-line {
            \wordwrap-string
              #"Testing horizontal behaviour at bend start.

It should not collide with the TabNoteHead.

The horizontal padding may be customized with an override for
\\BendSpanner.details.horizontal-left-padding"
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
