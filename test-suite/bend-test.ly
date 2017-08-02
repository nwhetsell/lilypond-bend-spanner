\version "2.19.46"

%\pointAndClickOff

%#(ly:set-option 'debug-skylines #t)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% include bend-files
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\include "../bend-helpers.ly"
\include "../bend-stencils.ly"
\include "../bend-grob-defs.ly"
\include "../bend-music-functions.ly"

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% layout for tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


\paper { indent = 0 }

\layout {
  \context {
    \Score
    \override RehearsalMark.self-alignment-X = #LEFT
  }
  \context {
    \Voice
    \omit StringNumber
  }
  \context {
    \TabVoice
    %% next three lines only for better viewing/debugging
    \override BendSpanner.color = #red
    \override BendSpanner.layer = #20
    \override TabNoteHead.whiteout = ##f
    \override BendSpanner.details.target-visibility = ##f
  }
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% include test-files
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\include "bends-consecutive.ly"
\include "bends-exclude.ly"
\include "bends-horizontal-spacing-at-bend-start.ly"
\include "bends-line-break-spacing.ly"
\include "bends-simple-bends-up-and-down-line-break.ly"
\include "bends-simple-bends-up-or-down-line-break.ly"
\include "bends-tab-full-notation.ly"
\include "bends-with-tied-notes.ly"
\include "bends-customize-arrow-head.ly"
\include "bends-customize-bend-amount.ly"
\include "bends-fontSize-changes.ly"
\include "bends-pre-bends.ly"


%\new TabVoice { ais\startBend b\stopBend }



%% current default
%% open strings are not bendable, neither starting nor ending a bend
%% it's possible to set single notes bendable
\new TabVoice
  {
  	  ais\startBend \tweak bend-me ##t b\stopBend
  	  <d ais>\startBend <d \tweak bend-me ##t b>\stopBend
  }

%% most brute-force
%% excluding notes from bend doesn't work any more
\new TabVoice
  \with { \remove \BendMeEngraver }
  { ais\startBend b\stopBend }

%% current default
%% but set all notes bendable in \with
\new TabVoice
  \with { \override TabNoteHead.bend-me = ##t }
  {
  	  ais\startBend b\stopBend
  	  <d ais>\startBend <d b>\stopBend
  }

%% set all notes bendable in \with
%% exclude single notes
%% make this the default?
\new TabVoice
  \with { \override TabNoteHead.bend-me = ##t }
  {
  	  ais\startBend  b\stopBend
  	  <\tweak bend-me ##f d ais>\startBend
  	  <\tweak bend-me ##f d b>\stopBend
  }
