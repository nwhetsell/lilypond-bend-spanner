\version "2.19.46"

%#(format #t "\ncompiling ----------- ~a in ~a.ly"
%  (car (ly:input-file-line-char-column (*location*)))
%  (ly:parser-output-name))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% simple and double bends up and down
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bend-up-and-down = {
  <>^"simple bends up and down"
  g,4\startBend ais,\stopBend\startBend g,2\stopBend
  c4\startBend dih\stopBend\startBend c2\stopBend
  f4\startBend g\stopBend\startBend f2\stopBend
  bes4\startBend bih \stopBend\startBend bes2\stopBend
  d'4\startBend e'\stopBend\startBend d'2\stopBend
  g'4\startBend a'\stopBend\startBend g'2\stopBend

  \break

  <>^"double bends up"
  <g, c>4\startBend <a, d>\stopBend\startBend <g, c>2\stopBend
  <g, f>4\startBend <ais, gis>\stopBend\startBend <g, f>2\stopBend
  <g, bes>4\startBend <a, c'>\stopBend\startBend <g, bes>2\stopBend
  <g, d'>4\startBend <ais, eis'>\stopBend\startBend <g, d'>2\stopBend
  <g, g'>4\startBend <ais, ais'>\stopBend\startBend <g, g'>2\stopBend
  <d' fis'>4\startBend <eis' gisis'>\stopBend\startBend <d' fis'>2\stopBend
  <d' fis'>4\startBend <eis' gisis'>\stopBend\startBend <fis' d'>2\stopBend

  \bar "|."
}

\bookpart {
  \score {
    <<
      \new Staff { \clef "G_8" \bend-up-and-down }
      \new TabVoice \bend-up-and-down
    >>
    \header {
      piece =
        \markup \rounded-box \fill-line {
              "Testing simple and double bends up and down"
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

bend-up-and-down-line-break = {
  <>^"simple bends up and down"
  g,2\startBend ais,\stopBend\startBend \break  g,1\stopBend
  g,1\startBend \break a,\stopBend\startBend g,1\stopBend
  g'2\startBend ais'\stopBend\startBend \break  g'1\stopBend
  g'1\startBend \break ais'\stopBend\startBend g'1\stopBend


  <d' fis'>2\startBend <eis' gisis'>\stopBend\startBend
  \break <d' fis'>1\stopBend

  <d' fis'>1\startBend
  \break <eis' gisis'>\stopBend\startBend <d' fis'>1\stopBend
  <d' fis'>1\startBend
  \break <eis' gisis'>\stopBend\startBend <d' fis'>1\stopBend
  <d' fis'>1\startBend
  \break <eis' gisis'>\stopBend\startBend <d' fis'>1\stopBend

  \bar "|."
}

\bookpart {
  \paper { ragged-right = ##t }
  \score {
    <<
      \new Staff { \clef "G_8" \bend-up-and-down-line-break }
      \new TabVoice \bend-up-and-down-line-break
    >>
    \header {
      piece =
        \markup \rounded-box \fill-line {
              "Testing line-breaking simple and double bends up and down"
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