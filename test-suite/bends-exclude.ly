\version "2.19.46"

%#(format #t "\ncompiling ----------- ~a in ~a.ly"
%  (car (ly:input-file-line-char-column (*location*)))
%  (ly:parser-output-name))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% bends can be excluded
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


mus-open-strings-exludes = {
  \accidentalStyle forget
  <>^"open strings get no bend unless indicated"

  %
  <b, d c' e'>2\startBend <d bih, cih' e'>2\stopBend
  c'2\startBend cih'\stopBend
  <
    b,
    \tweak bend-me ##t
    d
    c'
    \tweak bend-me ##t
    e'
  >2\startBend
    <dih bih, cih' eih'>2\stopBend

  %% simple bend up with a tied note on an open-string
  <g\4 g\3 d'\2 >4~\startBend
  <a\4 g\3 e'\2 >\stopBend

  %% simple bend down with a tied note on an open-string
  <a\4 g\3 e'\2 >4~\startBend
  %% REMARK Tie is assigned wrongly, thus the need to change input of the
  %% target-chord. It's a Tie-issue not a BendSpanners one!
  <g\3 g\4  d'\2 >4\stopBend

  %% simple bend up and down with a tied note on an open-string
  <g\4 g\3 d'\2 >4~\startBend
  <a\4 g\3 e'\2 >~\stopBend\startBend
  <g\3 g\4  d'\2 >2\stopBend

  <>^"set bend-me ##t for open strings to get a bend"
  %% simple bend up with an open-string
  <g\4 \tweak bend-me ##t g\3 d'\2 >4\startBend
  <gis\4 gis\3 dis'\2 >\stopBend

  %% simple bend down with an open-string
  <gis\4 gis\3 dis'\2 >4\startBend
  <g!\4 \tweak bend-me ##t g!\3 d'!\2 >\stopBend


  %% simple bend up and down
  <g\4 \tweak bend-me ##t g\3 d'\2 >4\startBend
  <gis\4 gis\3 dis'\2 >\stopBend\startBend
  <g!\4 \tweak bend-me ##t g!\3 d'!\2 >2\stopBend

  \break

  <>^"other notes excluded via \\tweak bend-me ##f"

  %% simple bend up
  %% excluding middle note from both chords, results in printing them both
  <g\4 \tweak bend-me ##f b\3 d'\2 >4\startBend
  <a\4 \tweak bend-me ##f cis'\3 e'\2 >\stopBend

  %% simple bend up
  %% excluding middle note from start-chord and no middle note in end-chord
  <g\4 \tweak bend-me ##f b\3 d'\2 >4\startBend
  <a\4 e'\2 >\stopBend

  %% simple bend down
  %% excluding middle note from both chords, results in printing them both
  <a\4 \tweak bend-me ##f cis'\3 e'\2 >4\startBend
  <g\4 \tweak bend-me ##f b\3  d'\2 >\stopBend

  %% simple bend down
  %% excluding middle note from start-chord and no middle note in end-chord
  <a\4 \tweak bend-me ##f cis'\3 e'\2 >4\startBend
  <g\4 d'\2 >\stopBend

  %% simple bend up and down
  <b\3 d'\2 \tweak bend-me ##f  e'\1 >4~\startBend
  <c'\3 es'\2 \tweak bend-me ##f e'\2 >\stopBend\startBend
  <b\3 d'\2 \tweak bend-me ##f e'\1 >2\stopBend

  \bar "|."
}

 mus-open-strings-exludes-line-breaks = {
   \accidentalStyle forget
   <>^"line-breaks"

   %% Remark: After line-break a tied tab-note-head is parenthesized per default

   <g\4 g\3 d'\2 >1~\startBend
   \break
   <a\4 g\3 e'\2 >~\stopBend\startBend
   \break
   <g\3 g\4  d'\2 >\stopBend


   <b\3 d'\2 \tweak bend-me ##f  e'\1 >1~\startBend
   \break
   <c'\3 es'\2 \tweak bend-me ##f e'\1 >\stopBend\startBend
   \break
   <b\3 d'\2 \tweak bend-me ##f e'\1 >\stopBend

   \bar "|."
 }

\bookpart {
  \score {
    <<
      \new Staff { \clef "G_8" \mus-open-strings-exludes }
      \new TabVoice \mus-open-strings-exludes
    >>
    \header {
      piece =
        \markup \rounded-box \fill-line {
          \wordwrap-string
            #"NoteHeads with the bend-me-property set #f don't get a bend.

Open strings are not bended by default, to do so set bend-me #t."

            \null
        }
    }
  }

  \score {
    <<
      \new Staff { \clef "G_8" \mus-open-strings-exludes-line-breaks }
      \new TabVoice \mus-open-strings-exludes-line-breaks
    >>
    \header {
      piece =
        \markup \rounded-box \fill-line {
          \wordwrap-string
            #"line-breaks with excluded bends"
            \null
        }
    }
    \layout { ragged-right = ##t }
  }
}
