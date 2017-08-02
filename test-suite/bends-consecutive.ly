\version "2.19.46"

%#(format #t "\ncompiling ----------- ~a in ~a.ly"
%  (car (ly:input-file-line-char-column (*location*)))
%  (ly:parser-output-name))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% consecutive bends up
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mus-consecutive-bend = {

  \mark "consecutive bend up"

  <c'\3-1 bes\4-4 a, e'>4 \startBend
    <d'\3-2>\stopBend \bendStencilTweak 2 \startBend
    e'\3 \stopBend


  c'4\3\startBend
    d'\3\stopBend \bendStencilTweak 2 \startBend
    e'\3\stopBend \bendStencilTweak 3 \startBend
    fis'\stopBend

  <f''\2 a''>4\startBend
    <g'' b''\1>\stopBend \bendStencilTweak 2 \startBend
    <a'' cis'''>\stopBend

  \mark "consecutive bend down"

  e'4\2 \bendStencilTweak 2 \startBend
    d'\stopBend  \startBend
    c'4\stopBend

  <a' cis''> \bendStencilTweak 2 \startBend
    <g' b'>\stopBend \startBend
    <f' a'>\stopBend


  e'4\3 \bendStencilTweak 3 \startBend
    d'\3\stopBend \bendStencilTweak 2 \startBend
    c'\3\stopBend  \startBend
    bes\3 \stopBend

  \mark "consecutive bend down with pre-bend"

  \grace c'4\3 -\tweak style #'pre-bend-hold \bendStencilTweak 2 \startBend
  e'4\3\stopBend \bendStencilTweak 2 \startBend
    d'\3\stopBend  \startBend
    c'4\3\stopBend

  \grace bes4\3 -\tweak style #'pre-bend-hold \bendStencilTweak 3 \startBend
  e'4\3\stopBend \bendStencilTweak 3 \startBend
    d'\3\stopBend \bendStencilTweak 2 \startBend
    c'\3\stopBend  \startBend
    bes\3 \stopBend

  \bar "|."
}

mus-consecutive-bend-line-break = {

  \mark "line-breaking consecutive bend up"

  c'1 \startBend
  \break
    d'\stopBend \bendStencilTweak 2 \startBend
    e'\stopBend

  c'1 \startBend
    d'\stopBend \bendStencilTweak 2 \startBend
  \break
    e'\stopBend

  \mark "line-breaking consecutive bend down"

  e'1 \bendStencilTweak 2 \startBend
  \break
    d'\stopBend  \startBend
    c'\stopBend

  e'1 \bendStencilTweak 2 \startBend
    d'\stopBend  \startBend
  \break
    c'\stopBend

  \bar "|."
}

\bookpart {
  \score {
    <<
      \new Staff { \clef "G_8" \mus-consecutive-bend }
      \new TabVoice \mus-consecutive-bend
    >>
    \header {
      piece =
        \markup \rounded-box \fill-line {
          \wordwrap-string #"Testing consecutive bends without line-break."
          \null
        }
    }
    \layout {
      \context {
        \TabStaff
          %minimumFret = #3
          %restrainOpenStrings = ##t
      }
    }
  }

  \score {
    <<
      \new Staff { \clef "G_8" \mus-consecutive-bend-line-break }
      \new TabVoice \mus-consecutive-bend-line-break
    >>
    \header {
      piece =
        \markup \rounded-box \fill-line {
          \wordwrap-string #"Testing consecutive line-breaking bends."
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