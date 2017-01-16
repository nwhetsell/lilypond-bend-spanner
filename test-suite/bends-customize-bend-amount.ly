\version "2.19.46"

%#(format #t "\ncompiling ----------- ~a in ~a.ly"
%  (car (ly:input-file-line-char-column (*location*)))
%  (ly:parser-output-name))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% arrow-head
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mus-bend-text-init = {
  a,4\6 \startBend b,\6\stopBend
  d'\startBend des'\stopBend
  cis'\startBend d'\stopBend\startBend cis'2\stopBend
  c'4\3\startBend
    d'\3\stopBend \bendStencilTweak 2 \startBend
    e'2\3 \stopBend

  e'4\2 \bendStencilTweak 2 \startBend
    d'\stopBend  \startBend
    c'2\stopBend
}

mus-bend-text = {

  \mark "default"
  \mus-bend-text-init

  \mark "text large, bold and italic"
  \override BendSpanner.font-size = 8
  \override BendSpanner.font-series = #'bold
  \override BendSpanner.font-shape = #'italic
  \mus-bend-text-init
  \revert BendSpanner.font-size
  \revert BendSpanner.font-series
  \revert BendSpanner.font-shape

  \mark "using font \"Purisa\" (make sure you have it)"
  \override BendSpanner.font-name = "Purisa"
  \mus-bend-text-init
  \revert BendSpanner.font-name

  \mark "details.bend-amount-strings changed "
  \override BendSpanner.font-size = #-4
  \override BendSpanner.font-shape = #'italic
  \override BendSpanner.details.bend-amount-strings =
   #'((quarter . "¼")
      (half . "½")
      (three-quarter . "¾")
      (full . "full"))
  f,4\6 \startBend fih,\6\stopBend
  f,4\6 \startBend fis,\6\stopBend
  f,4\6 \startBend fisih,\6\stopBend
  f,4\6 \startBend g,\6\stopBend
  f,4\6 \startBend gih,\6\stopBend
  f,4\6 \startBend gis,\6\stopBend
  f,4\6 \startBend gisih,\6\stopBend
  \revert BendSpanner.font-size
  \revert BendSpanner.font-shape
  \revert BendSpanner.details.bend-amount-strings

  \once \override BendSpanner.text = "foo"
  f,4\6 \startBend g,\6\stopBend

  %% Also possible:
  \once \override BendSpanner.after-line-breaking =
    #(lambda (grob)
      (ly:grob-set-property! grob 'text
        #{
          \markup
            \override #'(direction . 1)
            \override #'(baseline-skip . 2)
            \dir-column \halign #CENTER {
              #(ly:grob-property grob 'text) \fontsize #-2 "really?"
            }
        #}))

  f,4\6 \startBend f''\6\stopBend


  \bar "|."
}

\bookpart {
  \score {
    <<
      \new Staff { \clef "G_8" \mus-bend-text }
      \new TabStaff \mus-bend-text
      %% TODO not properly scaled:
      %\new TabStaff \with { fontSize = 6 } \mus-bend-text
    >>
    \header {
      piece =
        \markup \rounded-box \fill-line {
          \wordwrap-string
            #"Customize the markup indicating bend-amount.

BendSpanner responds to overrides for font-size, font-shape, font-series,
font-name.
Overriding details.bend-amount-strings will change generally which text is
printed.
Single overrides are possible going for the text-property
(or after-line-breaking, if needed).
"
            \null
        }
    }
    \layout {
      \context {
        \Score
        supportNonIntegerFret = ##t
        \override RehearsalMark.font-size = #-3
      }
    }
  }
}