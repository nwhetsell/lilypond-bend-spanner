\version "2.19.46"

%#(format #t "\ncompiling ----------- ~a in ~a.ly" 
%  (car (ly:input-file-line-char-column (*location*))) 
%  (ly:parser-output-name))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% bends with tied notes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mus-bends-with-ties = { 
%%{
  a'4~\startBend a'2 b'4\stopBend
  b'4~\startBend b'2 a'4\stopBend
  
  \once \override BendSpanner.style = #'pre-bend
                     
  \grace e'4~\startBend e'2. fis'4\stopBend
  
  \grace { 
    c'8-\tweak style #'pre-bend-hold \startBend d'\3~ 
  } 
  d'4~ d'\3\stopBend \startBend 
  c'2 \stopBend  
  
  c'2\3\startBend d'~\3\stopBend \bendHold \startBend d'\3\stopBend
  
  
  c'\3\startBend \break d'~\3\stopBend \bendHold \startBend d'\3\stopBend
  
  
  c'\3\startBend d'~\3\stopBend \bendHold \startBend \break d'\3\stopBend

  c'\3\startBend d'~\3\stopBend \bendHold \startBend d'\3~ 
    d'\3\stopBend\startBend c'\3\stopBend
    
  r2
  
  c'\3\startBend d'~\3\stopBend \bendHold \startBend d'\3~ 
    \break d'\3\stopBend\startBend 
    c'\3\stopBend
%}    
  e'
  b
  
  \bar "|."
}

\bookpart {
  \score {
    <<
      \new Staff { \clef "G_8" \mus-bends-with-ties }
      \new TabVoice { 
        \mark "bends up/down, pre-bends with and without \"release\"" 
        \mus-bends-with-ties 
      }
    >>
    \header { 
      piece = 
        \markup \rounded-box \fill-line { \wordwrap-string 
          #"Testing bends with tied notes. 
          
All covered NoteColumns are transparent.
" 
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