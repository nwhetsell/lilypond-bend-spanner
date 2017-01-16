\version "2.19.46"

%#(format #t "\ncompiling ----------- ~a in ~a.ly"
%  (car (ly:input-file-line-char-column (*location*)))
%  (ly:parser-output-name))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% arrow-head
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% An override for details.arrow-stencil needs five arguments representing the
%% original procedure's arguments, i.e.
%% thickness end-curve-coords height width dir
%% In the example below they are introduced most simple as a, b, c, d, e
%% Don't forget the outer argument, i.e. grob
%% It should be assigned to 'after-line-breaking
%% It may be needed to adjust details.bend-arrowhead-height as well as the
%% stencil-translation-y-value


%% A simple test
#(define set-test-stencil!
  (lambda (grob)
    (ly:grob-set-nested-property! grob '(details arrow-stencil)
      (lambda (a b c d e) ;; thickness end-curve-coords height width dir
        (ly:stencil-translate
          (grob-interpret-markup
            grob
            #{
               \markup
                 \box
                 \scale #(cons 1 e)
                 \raise #0.3
                 \halign #CENTER
                 \fontsize #-2
                 "x"
            #})
          b)))))

%% switch to arrow-heads from feta
%% REMARK the result is not overwhelming, it does not scale properly with
%% changed fontSize, a TODO.
%% Maybe a better result happens if the arrow-heads are slightly rotated,
%% relying on the curve's control-points
#(define (feta-arrow-head dir)
  (lambda (grob)
    (let* ((layout (ly:grob-layout grob))
           (props (ly:grob-alist-chain grob))
           (font (ly:paper-get-font layout (cons '((font-encoding . fetaMusic)
                                                   (font-name . #f))
                                                 props)))
           ;(font-size (ly:grob-property grob 'font-size))
           ;(size-factor (magstep font-size))
           ;(blot (ly:output-def-lookup layout 'blot-diameter))
           (glyph-string
             (format #f "arrowheads.open.1~a1" (if (> dir -1) "" "M"))))
      (ly:font-get-glyph font glyph-string))))

#(define set-default-arrow-glyph-stencil!
  (lambda (grob)
    (ly:grob-set-nested-property! grob '(details arrow-stencil)
      (lambda (a b c d e) ;; thickness end-curve-coords height width dir
        (let* ((new-stil ((feta-arrow-head e) grob))
               (new-stil-y-ext (ly:stencil-extent new-stil Y))
               (new-stil-y-length (interval-length new-stil-y-ext))
               ;(staff-space (ly:staff-symbol-staff-space grob))
               (font-size (ly:grob-property grob 'font-size))
               (size-factor (magstep font-size))
               (thick (ly:grob-property grob 'thickness))
               (staff-symbol-line-thickness
                 (ly:staff-symbol-line-thickness grob))

               (bend-line-thickness
                 (* staff-symbol-line-thickness
                    size-factor
                    (ly:grob-property grob 'thickness))))
        (ly:grob-set-nested-property! grob '(details bend-arrowhead-height)
            (if (> e -1)
                (- (cdr new-stil-y-ext) bend-line-thickness)
                (+ (car new-stil-y-ext) bend-line-thickness)))

        (ly:stencil-translate
          new-stil
          (cons
            (car b)
            (+ (cdr b)
               (* e new-stil-y-length)))))))))

mus-arrow-heads-init = {
  b,4\5\startBend c\5\stopBend
  a,4\6\startBend b,4---1\6\stopBend
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

mus-arrow-heads = {

  \mark "default arrow-heads"
  \mus-arrow-heads-init

  \mark "more height and width"
  \temporary \override BendSpanner.details.bend-arrowhead-height = 1.8
  \temporary \override BendSpanner.details.bend-arrowhead-width = 0.6
  \mus-arrow-heads-init
  \revert BendSpanner.details.bend-arrowhead-height
  \revert BendSpanner.details.bend-arrowhead-width

  \mark "less height and more width"
  \temporary \override BendSpanner.details.bend-arrowhead-height = 0.8
  \temporary \override BendSpanner.details.bend-arrowhead-width = 1.0
  \mus-arrow-heads-init
  \revert BendSpanner.details.bend-arrowhead-height
  \revert BendSpanner.details.bend-arrowhead-width

  \mark "test-stencil"
  \override BendSpanner.after-line-breaking = #set-test-stencil!
  \mus-arrow-heads-init
  \revert BendSpanner.after-line-breaking

  \mark "use arrow-heads from feta"
  \override BendSpanner.after-line-breaking = #set-default-arrow-glyph-stencil!
  \mus-arrow-heads-init
  \revert BendSpanner.after-line-breaking

  \bar ".|"
}

\bookpart {
  \score {
    <<
      \new Staff { \clef "G_8" \mus-arrow-heads }
      \new TabStaff \mus-arrow-heads
      %% TODO not properly scaled:
      %\new TabStaff \with { fontSize = 6 } \mus-arrow-heads
    >>
    \header {
      piece =
        \markup \rounded-box \fill-line {
          \wordwrap-string
            #"Change arrow-head

TODO does not scale properly with changed fontSize. Slightly rotate the
feta-arrow-heads?"
            \null
        }
    }
  }
}