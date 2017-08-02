\version "2.19.46"

%#(format #t "\ncompiling ----------- ~a in ~a.ly"
%  (car (ly:input-file-line-char-column (*location*)))
%  (ly:parser-output-name))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% pre-bends
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mus-pre-bend = {
  \override BendSpanner.style = #'pre-bend
  \override BendSpanner.details.head-text-break-visibility = ##(#t #t #f)

  \grace a'4\startBend b'4\stopBend
  \grace e'4\2\startBend fis'4\stopBend
  \grace c'4\startBend d'4\stopBend
  \grace g4\4\startBend a4\stopBend
  \grace d4\5\startBend e4\stopBend
  \grace a,4\6\startBend b,4\stopBend

  \grace <f' a'>4\startBend <g' b'>4\stopBend
  \grace <c'\3 e'\2>4\startBend <d' fis'>4\stopBend
  \grace <a c'>4\startBend <b d'>4\stopBend
  \grace <e\5 g\4>4\startBend <fis a>4\stopBend
  \grace <b,\6 d\5>4\startBend <c e>4\stopBend
  \grace <b,\6 d^~ >4\startBend <c d>4\stopBend

  \bar "|."
}

mus-pre-bend-line-break = {

  a'1\preBend\startBend \break b'\stopBend

  \bar "|."
}

\bookpart {
  \score {
    <<
      \new Staff { \clef "G_8" \mus-pre-bend }
      \new TabVoice { \mark "default pre-bends" \mus-pre-bend }
    >>
    \header {
      piece =
        \markup \rounded-box \wordwrap-string
          #"Testing pre-bends.

They start with a little gap above the
TabNoteHead, this works for changed staff-space as well.

The vertical padding may be customized with an override for
\\BendSpanner.details.vertical-padding.

The bend for pre-bend and pre-bend-hold always starts at the top-most note-head,
even if it's a not bended open string."
    }
  }

  \score {
    <<
      \new Staff { \clef "G_8" \mus-pre-bend }
      \new TabVoice {
        \mark "default pre-bends with changed staff-space" \mus-pre-bend
      }
    >>
    \layout { \override TabStaff.StaffSymbol.staff-space = 3 }
  }

  \score {
    <<
      \new Staff { \clef "G_8" \mus-pre-bend }
      \new TabVoice {
        \mark "default pre-bends with vertical-padding set 0.4" \mus-pre-bend
      }
    >>
    \layout { \override BendSpanner.details.vertical-padding = 0.4 }
  }

  \score {
    <<
      \new Staff { \clef "G_8" \mus-pre-bend-line-break }
      \new TabVoice {
        \mark "broken pre-bends" \mus-pre-bend-line-break
      }
    >>
    \layout { ragged-right = ##t }
  }
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% pre-bends-hold
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mus-pre-bend-hold = {
  \grace {
    c'8\preBendHold \startBend d'\stopBend \startBend
  }
  c'2 \stopBend

  \grace {
    < \tweak id "no-bend" g~ c'>8\preBendHold \startBend
      <g d' >\stopBend\startBend
  }
  <g c'>2\stopBend

  \grace {
    < \tweak id "bend" g c'>8\preBendHold \startBend
      <a d' >\stopBend\startBend
  }
  <g c'>2\stopBend

  <g c'>2-\preBendHold \startBend

  \break

  <a d'>4\stopBend\startBend <g c'>\stopBend

  <g c'>-\preBendHold \startBend <a d'>\stopBend\startBend
  \break
  <g c'>4\stopBend

  r2
  < \tweak id "bend" g c'>4-\preBendHold\startBend
  \break
  <a d'>4\stopBend\startBend <g c'>\stopBend
  \bar "|."
}

\bookpart {
  \paper { ragged-right = ##t }
  \score {
    <<
      \new Staff { \clef "G_8" \mus-pre-bend-hold }
      \new TabVoice \mus-pre-bend-hold
    >>
    \header {
      piece =
        \markup \rounded-box \fill-line { \wordwrap-string
        #"Testing pre-bend-hold:

simple, from chords, line-break."
        \null
        }
    }
  }
}









