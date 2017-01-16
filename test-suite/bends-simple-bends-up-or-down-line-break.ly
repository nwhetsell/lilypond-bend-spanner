\version "2.19.46"

%#(format #t "\ncompiling ----------- ~a in ~a.ly"
%  (car (ly:input-file-line-char-column (*location*)))
%  (ly:parser-output-name))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% simple and double bends up and down
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simple-bends = {
  <>^"simple bends up"
  g,4\startBend ais,\stopBend
  c\startBend dih\stopBend
  f\startBend g\stopBend
  bes\startBend bih \stopBend
  d'\startBend e'\stopBend
  g'\startBend a'\stopBend


  <>^"simple bends down"
  ais,\6\startBend g,\stopBend
  dis\5\startBend c\stopBend
  g\4\startBend f\stopBend
  bis\3\startBend bes \stopBend
  e'\2\startBend d'\stopBend
  a'\1\startBend g'\stopBend

  \break

  <>^"double bends up"
  <g, c>\startBend <ais, d>\stopBend
  <g, f>\startBend <ais, g>\stopBend
  <g, bes>\startBend <ais, c'>\stopBend
  <g, d'>\startBend <ais, e'>\stopBend
  <g, g'>\startBend <ais, ais'>\stopBend
  <d' fis'>\startBend <eis' gisis'>\stopBend


  <>^"double bends down"
  <ais, d>\startBend <g, c>\stopBend
  <ais, g>\startBend <g, f>\stopBend
  <ais, c'>\startBend <g, bes>\stopBend
  <ais, e'>\startBend <g, d'>\stopBend
  <ais, ais'>\startBend <g, g'>\stopBend
  <eis' gisis'>\startBend <d' fis'>\stopBend

  \bar "|."
}

\bookpart {
  \score {
    <<
      \new Staff { \clef "G_8" \simple-bends }
      \new TabVoice \simple-bends
    >>
    \header {
      piece =
        \markup \rounded-box \fill-line {
              "Testing simple and double bends up or down"
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% line-breaking simple and double bends up and down
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simple-bends-line-break = {
  <>^"simple bends up"
  g,1\startBend \break ais,\stopBend
  c\startBend \break dih\stopBend
  f\startBend \break g\stopBend
  bes\startBend \break bih \stopBend
  d'\startBend \break e'\stopBend
  g'\startBend \break a'\stopBend


  <>^"simple bends down"
  ais,\6\startBend \break g,\stopBend
  dis\5\startBend \break c\stopBend
  g\4\startBend \break f\stopBend
  bis\3\startBend \break bes \stopBend
  e'\2\startBend \break d'\stopBend
  a'\1\startBend \break g'\stopBend

  \break

  <>^"double bends up"
  <g, c>\startBend \break <ais, d>\stopBend
  <g, f>\startBend \break <ais, g>\stopBend
  <g, bes>\startBend \break <ais, c'>\stopBend
  <g, d'>\startBend \break <ais, e'>\stopBend
  <g, g'>\startBend \break <ais, ais'>\stopBend
  <d' fis'>\startBend \break <eis' gisis'>\stopBend


  <>^"double bends down"
  <ais, d>\startBend \break <g, c>\stopBend
  <ais, g>\startBend \break <g, f>\stopBend
  <ais, c'>\startBend \break <g, bes>\stopBend
  <ais, e'>\startBend \break <g, d'>\stopBend
  <ais, ais'>\startBend \break <g, g'>\stopBend
  <eis' gisis'>\startBend \break <d' fis'>\stopBend

  \bar "|."
}

\bookpart {
  \score {
    <<
      \new Staff { \clef "G_8" \simple-bends-line-break }
      \new TabVoice \simple-bends-line-break
    >>
    \header {
      piece =
        \markup \rounded-box \fill-line {
              "Testing line-breaking simple and double bends up or down"
              \null
        }
    }
    \layout {
      ragged-right = ##t
      \context {
        \TabStaff
          minimumFret = #3
          restrainOpenStrings = ##t
      }
    }
  }
}