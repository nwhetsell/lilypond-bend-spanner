\version "2.19.46"

#(define (quarterdiff->string alist)
  "Takes a number, which is supposed to be a  number of quarter-steps.
Returns a fraction-string like ¾, maybe a mixed number-string like 1½
The actual formating relies on the settings in @var{alist}."
  (lambda (quarterdiff)
    (let ((wholesteps (floor (/ quarterdiff 4))))
      (string-append
        (case wholesteps
          ((0) "")
          ((1) (if (and (zero? (modulo quarterdiff 4)) (assoc-get 'full alist))
                   (assoc-get 'full alist)
                   (number->string wholesteps)))
          (else (number->string wholesteps)))
        (case (modulo quarterdiff 4)
          ((1) (or (assoc-get 'quarter alist) "¼"))
          ;((1) "|")
          ((2) (or (assoc-get 'half alist) "½"))
          ((3) (or (assoc-get 'three-quarter alist)"¾"))
          (else ""))))))

#(define (get-quarter-diffs pitch-list-pair)
    "Takes @var{pitch-list-pair}, which is supposed to be a pair of lists, both
containing pitches, previously sorted with @code{ly:pitch<?}.
Returns a quarter-tone-diff-number calculated from the highest pitches from the
two sub-lists."

;; Bendings to different amounts are very unlikely, so we take the last pitch
;; of every sublist for comparison.
;; Relies on sorted sublists via @code{ly:pitch<?} and both not empty
  (let* ((first-pitch-list (car pitch-list-pair))
         (second-pitch-list (cdr pitch-list-pair))
         (highest-first (last first-pitch-list))
         (highest-second (last second-pitch-list)))
    (- (ly:pitch-quartertones highest-second)
       (ly:pitch-quartertones highest-first))))

#(define (bend::target-cautionary spanner)
  "Sets 'display-cautionary of all note heads of spanners right bound.  Needs to
be done 'before-line-breaking"
  (let* ((all-left-right-note-heads (get-bound-note-heads spanner))
         (right-note-heads
           (bend::remove-certain-tab-note-heads
             (cdr all-left-right-note-heads))))
;; can't see a way to avoid `bend::remove-certain-tab-note-heads' being called
;; `bend::target-cautionary' is used before-line-breaking in grob-def
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(newline)
;(write-me "bend::remove-certain-tab-note-heads is called in\t"
;          'bend::target-cautionary)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (for-each
       (lambda (right-tab-nh)
         (ly:grob-set-property! right-tab-nh 'display-cautionary #t))
       right-note-heads)))

#(define (bend::text-string spanner)
;(pretty-print left-right-pitches)
  "Takes a spanner-grob, calculates a list with the quarter-tone-diffs between
the pitches of starting and ending bound.  Returns only the first element of
this list as a string, because bending to different amounts is not likely."
  (let* ((sorted-left-right-pitches (get-pitches-from-bound-note-heads spanner))
         (quarter-diffs (get-quarter-diffs sorted-left-right-pitches))
         (bend-amount-strings
           (assoc-get
             'bend-amount-strings
             (ly:grob-property spanner 'details)
             '()))
         (bend-value
           ((quarterdiff->string bend-amount-strings) quarter-diffs)))
    bend-value))

#(define (bend::remove-certain-tab-note-heads tab-note-heads)
  "Takes the list @var{tab-note-heads} and removes all note-heads, which
are played on open strings, unless grob-property @code{bend-me} is set
@code{#t}.
Other note-heads with @code{bend-me} set @code{#f} are removed as well."
  (remove
    (lambda (tnh) (not (ly:grob-property tnh 'bend-me)))
    tab-note-heads))

#(define (get-bound-note-heads spanner)
  "Takes a spanner-grob and returns a nested list containing the all note-heads
of the intial starting and the final NoteColumn."
  (let* ((orig (ly:grob-original spanner))
         (siblings (if (ly:grob? orig)
                       (ly:spanner-broken-into orig)
                       '()))
         ;; get the bend-spanner's starting/ending NoteColumns
         (left-bound
            (ly:spanner-bound
              (if (not (null? siblings)) (car siblings) spanner)
              LEFT))
         (right-bound
           (ly:spanner-bound
             (if (not (null? siblings)) (last siblings) spanner)
             RIGHT))
         ;; get their NoteHeads
         (left-note-heads-array
           (ly:grob-object left-bound 'note-heads))
         (right-note-heads-array
           (ly:grob-object right-bound 'note-heads)))
    (if (and (ly:grob-array? left-note-heads-array)
             (ly:grob-array? right-note-heads-array))
        (cons
          (ly:grob-array->list left-note-heads-array)
          (ly:grob-array->list right-note-heads-array))
        ;; TODO display proper location
        ;; sth at the lines of (*location*) ...
        (ly:error
          "spanner ~a needs note heads as bounds: ~a"
          spanner
          (list left-note-heads-array right-note-heads-array)))))

%%%% TODO check behaviour for different string-tunings
#(define (get-top-most-tab-head tab-heads-list)
  "Looking at 'staff-position, get the highest tab-note-head
from @var{tab-heads-list}"
  (last
    (sort
      tab-heads-list
      (lambda (tnh1 tnh2)
        (<
           (ly:grob-property tnh1 'staff-position)
           (ly:grob-property tnh2 'staff-position))))))

#(define (get-pitches-from-bound-note-heads spanner)
  "Takes a spanner-grob, gets the note heads from the starting and ending bound,
applying @code{get-bound-note-heads}, removes not spanned ones, gets the pitches
of the note heads of each bound, sorts them with @code{ly:pitch<?} and returns
them as a nested list."
  (let* ((all-left-right-note-heads (get-bound-note-heads spanner))
         (left-right-note-heads-list
            (cons
              (bend::remove-certain-tab-note-heads
                (car all-left-right-note-heads))
              (bend::remove-certain-tab-note-heads
                (cdr all-left-right-note-heads))))
         (left-pitches
           (map
             (lambda (note-head)
               (ly:event-property (event-cause note-head) 'pitch))
             (car left-right-note-heads-list)))
         (sorted-left-pitches (sort left-pitches ly:pitch<?))
         (right-pitches
           (map
             (lambda (note-head)
               (ly:event-property (event-cause note-head) 'pitch))
             (cdr left-right-note-heads-list)))
         (sorted-right-pitches (sort right-pitches ly:pitch<?)))
    (cons sorted-left-pitches sorted-right-pitches)))

#(define (make-tab-heads-transparent tab-heads)
  "Set @code{transparent} @code{#t} in @code{TabVoice} for the bends target-
note-head. If \\tabFullNotation is set, the stem and flag will be transparent as
weel.
Doesn't work for beams.
TODO Needs design decision whether beams should be transparent or not.
If yes, a different approach is needed."
  (for-each
    (lambda (tab-note-head)
      (if (grob::has-interface tab-note-head 'tab-note-head-interface)
          (let* ((stem (ly:grob-object tab-note-head 'stem))
                 (flag (ly:grob-object stem 'flag))
                 (dot (ly:grob-object tab-note-head 'dot)))
            ;; Does not work
            ;(ly:grob-set-property! stem 'beaming '(#f . #f))
            (if (ly:grob? stem) (ly:grob-set-property! stem 'transparent #t))
            (if (ly:grob? flag) (ly:grob-set-property! flag 'transparent #t))
            (if (ly:grob? dot) (ly:grob-set-property! dot 'transparent #t))
            (ly:grob-set-property! tab-note-head 'transparent #t))))
    tab-heads))

#(define (bend::calc-bend-x-end
           bend-spanner bend-siblings top-left-tab-nhd top-right-tab-nhd)
  "Calculates the ending X-Coordinate of the bend-spanner.  At line end take
the items of BreakAlignGroup into account and a little padding.  Ends an
unbroken or the last of a broken spanner in the middle of the top-most
note-head of its bounding note-column."
  (let ((top-right-tab-nhd-x-ext
          (ly:grob-extent top-right-tab-nhd top-right-tab-nhd X))
        (curve-x-padding-line-end
          (assoc-get
            'curve-x-padding-line-end
            (ly:grob-property bend-spanner 'details)
            0)))
    (if (or (and (not (null? bend-siblings))
                 (equal? bend-spanner (car bend-siblings)))
            (null? bend-siblings))
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; unbroken or first of broken bend-spanner
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; For the first part of a broken bend-spanner ensure avoiding items of
        ;; BreakAlignGroup by taking its most left item's coordinate into the
        ;; calculation.
        (let* ((sys (ly:grob-system bend-spanner))
               (right-bound
                 (ly:spanner-bound bend-spanner RIGHT))
               (right-bound-elts
                 (list-copy
                   (ly:grob-array->list
                     (ly:grob-object right-bound 'elements))))
               (right-break-align-grobs
                 (if (grob::has-interface right-bound 'paper-column-interface)
                     (filter
                        (lambda (g)
                          (grob::has-interface g 'break-aligned-interface))
                        right-bound-elts)
                     (list right-bound)))
               (right-break-align-grobs-left-most
                 (car
                   (sort
                     right-break-align-grobs
                     (lambda (g1 g2)
                       (<
                          (ly:grob-relative-coordinate g1 sys X)
                          (ly:grob-relative-coordinate g2 sys X))))))
               (right-ref-grob-coord
                 (ly:grob-relative-coordinate
                   right-break-align-grobs-left-most sys X))
               (left-bound
                 (ly:spanner-bound bend-spanner LEFT))
               (left-bound-x-coord
                  (if (ly:grob? left-bound)
                      (ly:grob-relative-coordinate
                        left-bound
                        (ly:grob-common-refpoint left-bound sys X)
                        X)
                      0)))
          (+
            (-
               right-ref-grob-coord
               left-bound-x-coord
               (if (null? bend-siblings) 0 curve-x-padding-line-end))
            ;; ensure it ends in the middle of the top-most TabNoteHead of the
            ;; right-bounding NoteColumn, if any.
            (if (null? bend-siblings)
                (interval-center top-right-tab-nhd-x-ext)
                0)))
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; last of broken bend-spanner
        ;; with current coding a single BendSpanner can't cover two line-breaks
        ;; TODO Not, true! Take long ties notes into account, they _can_ span
        ;; several lines!!
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; simply take the right-bound coordinate
        (let* ((sys (ly:grob-system bend-spanner))
               (right-bound
                 (ly:spanner-bound bend-spanner RIGHT))
               (right-bound-x-coord
                  (if (ly:grob? right-bound)
                      (ly:grob-relative-coordinate
                        right-bound
                        sys
                        X)
                      0)))
          (+ right-bound-x-coord (interval-center top-right-tab-nhd-x-ext))))))

#(define (bend::calc-bend-x-begin
           bend-spanner
           bend-siblings
           bounding-noteheads
           factor
           quarter-tone-diffs)
  "Calculates the starting value in X-direction of the bend.
After a line-break, the values from the right-bound are taken minus 1.5
staff-spaces.
For bends-down or if grob-property 'style equals to 'pre-bend, 'hold or
'pre-bend-hold, @code{interval-center} is applied the top-most notehead of the
starting note-heads.
In any other case the right edge of the starting note-head is used. The value
of @code{BendSpanner.details.horizontal-left-padding} is added, which may be
changed by an approppiate override.
Returns a list of same length as the amount of bend-starting note-heads."
  (let* ((staff-space (ly:staff-symbol-staff-space bend-spanner))
         (left-note-heads-list-length (length (car bounding-noteheads)))
         (style (ly:grob-property bend-spanner 'style))
         (horizontal-left-padding
           (assoc-get
             'horizontal-left-padding
             (ly:grob-property bend-spanner 'details 0)))
         (top-left-tab-nhd (get-top-most-tab-head (car bounding-noteheads)))
         (top-right-tab-nhd (get-top-most-tab-head (cdr bounding-noteheads)))
         (top-left-tab-nhd-x-ext
           (ly:grob-extent top-left-tab-nhd top-left-tab-nhd X)))
    (cond ((and (not (null? bend-siblings))
                (equal? bend-spanner (last bend-siblings)))
           (make-list
             left-note-heads-list-length
             (- (bend::calc-bend-x-end
                  bend-spanner bend-siblings top-left-tab-nhd top-right-tab-nhd)
                (* 1.5 staff-space))))
          ((or (negative? quarter-tone-diffs)
               (eq? style 'pre-bend)
               (eq? style 'pre-bend-hold)
               (eq? style 'hold)
               (> factor 1))
           (make-list
             left-note-heads-list-length
             (interval-center top-left-tab-nhd-x-ext)))
          (else
              (map
                (lambda (tnh)
                  (+ (cdr (ly:grob-extent tnh tnh X)) horizontal-left-padding))
                (car bounding-noteheads))))))

#(define* (bend::calc-start-end-y-coords
            bend-spanner staff-space tab-note-heads
            #:optional quarter-tones-diffs)
  "Calculates Y-coordinate of the bend-spanners start in relation to
@var{tab-note-heads}.
For style 'pre-bend or 'pre-bend-hold or if the bend points down the resulting
value is offset to the top of the top-most note-head. In this case some vertical
padding is added, taken from the bend-spanners details-property
'vertical-padding.
Otherwise the vertical padding is taken from the property
details.y-distance-from-staffline-to-arrow"
  (let* ((vertical-padding
           (assoc-get
             'vertical-padding
             (ly:grob-property bend-spanner 'details)))
         (style (ly:grob-property bend-spanner 'style))
         (quarter-tones (if quarter-tones-diffs (list quarter-tones-diffs) '()))
         (top-tab-note-head (get-top-most-tab-head tab-note-heads))
         (important-tab-note-heads
           (if (or (eq? style 'pre-bend)
                   (eq? style 'pre-bend-hold)
                   (and quarter-tones-diffs (negative? quarter-tones-diffs))
                   (not quarter-tones-diffs))
               (list top-tab-note-head)
               tab-note-heads)))
    (sort
      (map
        (lambda (tab-nh)
          (+
             (* (/ (ly:grob-property tab-nh 'staff-position) 2)
                staff-space)
             (if (or (eq? style 'pre-bend)
                     (eq? style 'pre-bend-hold)
                     (and quarter-tones-diffs (negative? quarter-tones-diffs))
                     (not quarter-tones-diffs))
                 (+
                   (cdr
                     (ly:grob-extent
                       top-tab-note-head
                       top-tab-note-head
                       Y))
                   vertical-padding)
                 (* vertical-padding staff-space))))
        important-tab-note-heads)
       <)))