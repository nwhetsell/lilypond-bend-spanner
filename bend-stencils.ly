\version "2.19.46"

#(define (bend::text-stencil x y text)
  "Returns a stencil which prints the bends amount, translated to the end of
the bends arrow-head, given by @var{x} and @var{y}.  A little vertical padding
is added."
  (lambda (grob)
    (let* ((layout (ly:grob-layout grob))
           (props (ly:grob-alist-chain grob))
           (staff-space (ly:staff-symbol-staff-space grob))
           (font-size (ly:grob-property grob 'font-size 0.0))
           (scale-factor (magstep font-size))
           (vertical-padding
             (assoc-get
               'vertical-padding
               (ly:grob-property grob 'details))))
      (ly:stencil-translate
        (ly:stencil-aligned-to
          (interpret-markup layout props text)
          X CENTER)
        ;; double vertical-padding is my choice
        ;; TODO let it rely on a separate details-property?
        (cons x (+ y (* scale-factor vertical-padding 2)))))))

#(define (bend::arrow-head-stencil
            thickness end-curve-coords height width dir)
  "Returns an arrow-head-stencil. Calculated from the given dimensions,
translated to @var{end-curve-coords}, the end of the bends (curved) line."
  (let* ((pts-list
           (list
             ;; horizontal-left point, x and y coord
             (/ width -2) 0
             ;; horizontal-right point, x and y coord
             (/ width 2) 0
             ;; arrow-top point, x and y coord
             0 (* height dir))))
    (ly:stencil-translate
      (ly:make-stencil
        `(polygon ',pts-list ,thickness #t)
        (interval-widen (cons (/ width -2) (/ width 2)) (/ thickness 2))
        (interval-widen (ordered-cons 0 (* height dir)) (/ thickness 2)))
      end-curve-coords)))



#(define* (bend::make-line-curve-stencil
            thickness points-list #:optional line-style)
  "Print a line which may be continued with a curve.  If the line is horizontal
a dashed line is returned, relying on @var{line-style}"
  (let* ((command-list
           (case (length points-list)
            ((4)
              `(moveto
                  ,(list-ref points-list 0)
                  ,(list-ref points-list 1)
                lineto
                  ,(list-ref points-list 2)
                  ,(list-ref points-list 3)))
            ((10)
             `(moveto
                 ,(list-ref points-list 0)
                 ,(list-ref points-list 1)
               lineto
                 ,(list-ref points-list 2)
                 ,(list-ref points-list 3)
               curveto
                 ,(list-ref points-list 4)
                 ,(list-ref points-list 5)
                 ,(list-ref points-list 6)
                 ,(list-ref points-list 7)
                 ,(list-ref points-list 8)
                 ,(list-ref points-list 9)))
            (else (ly:error "list-length ~a needs to have 4 or 10 elements"))))
         (path-stencil
           (make-path-stencil
             command-list
             thickness
             1 1 #f)))
    ;; For a horizontal line and proper settings of line-style return a
    ;; translated dashed-line-stencil, otherwise use the `path-stencil'
    (if (and (= (list-ref points-list 1) (list-ref points-list 3))
             (= 4 (length points-list))
             (list? line-style)
             (= 4 (length line-style))
             (eq? (car line-style) 'dashed))
        (let ((x-end (- (list-ref points-list 2) (list-ref points-list 0))))
          (ly:stencil-translate
            (ly:make-stencil
              (list
                 'dashed-line
                 thickness
                 (cadr line-style) ;; on
                 (caddr line-style) ; off
                 x-end ;; x-end
                 0 ;; y-end
                 (last line-style)) ;; phase
              ;; x-ext
              (cons 0 x-end)
              ;; y-ext
              (cons (/ thickness -2) (/ thickness 2)))
            (cons (list-ref points-list 0) (list-ref points-list 1))))
        path-stencil)))

#(define (bend::curve-stencils-list
           thickness y-padding begin-x middle-x end-x begin-y end-y)
  "Returns a list of line-curve-stencil(s).  All curves reach the same
end-point, except for line-break.  At end of line the end points have descendin
y-coordinates."
  (if (not (list? begin-y))
      (list
        (bend::make-line-curve-stencil
          thickness (list begin-x begin-y end-x end-y)))
      ;; begin-x, begin-y and arrow-y are lists
      ;; TODO is this assumption too optimistic?
      (map
        (lambda (beg-x beg-y ending-y)
          (let* ((lst
                   (list
                     ;; moveto
                     beg-x beg-y
                     ;; lineto
                     middle-x beg-y
                     ;; curveto
                     middle-x beg-y end-x beg-y end-x ending-y)))
         (bend::make-line-curve-stencil thickness lst)))
       begin-x
       begin-y
       (if (zero? y-padding) ;; i.e. no line-break
           end-y
           ;; otherwise return a list with descending values
           (reverse
             (map
               (lambda (y-end i) (- y-end (* i y-padding)))
               end-y
               (iota (length begin-y))))))))

#(define (bend::draw-curves thickness begin-x middle-x end-x begin-y end-y)
  "Returns the combined stencils created by @code{bend::curve-stencils-list}
All stencils let room to place an arrow-head between them and the bend-amount-
indication.  The descending y-value of end-point for the curves is taken from
@code{details.curve-y-padding-line-end}."
  (lambda (grob)
    (let* ((orig (ly:grob-original grob))
           (siblings (if (ly:grob? orig)
                         (ly:spanner-broken-into orig)
                         '()))
           (font-size (ly:grob-property grob 'font-size 0.0))
           (scale-factor (magstep font-size))
           (bend-up?
             (if (list? begin-y)
                 (every (lambda (e) (> end-y e)) begin-y)
                 (> end-y begin-y)))
           (bend-arrowhead-height 
             (* scale-factor
               (assoc-get
                 'bend-arrowhead-height
                 (ly:grob-property grob 'details))))
           (curve-line-end-y-padding
             (if (and (not (null? siblings)) (equal? grob (car siblings)))
                 (assoc-get
                   'curve-y-padding-line-end
                   (ly:grob-property grob 'details)
                   0)
                 0))
           ;; recalculate `end-y' to make room for the arrow-head
           (new-end-y 
             (+ end-y
                (if bend-up? (- bend-arrowhead-height) bend-arrowhead-height)))
           (curve-stils
             (apply bend::curve-stencils-list
               (list thickness
                     curve-line-end-y-padding
                     begin-x
                     middle-x
                     end-x
                     begin-y
                     (if (list? begin-y)
                         (make-list (length begin-y) new-end-y)
                         new-end-y))))
           (curve-arrow-stil (apply ly:stencil-add curve-stils)))
        curve-arrow-stil)))
        
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% bend stencil, printing line, curve, arrow-head and text
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#(define* ((bend::draw-bend-arrow-stencil #:optional (factor 1)) grob)
  "Returns the final stencil. A line with a curve, an arrow-head and a value."
  (let* (;; We need to get _all_ bounding tab-note-heads to calculate the 
         ;; correct vertical position of the end of a down-spanner not only the
         ;; ones which actually starts a bend. 
         ;; This is important, if the top-most string is not bended.
         ;; But for creating the bend-stencil(s) we only need those which are
         ;; actually bended.
         ;; If after selecting no note-heads remain, print a warning and suicide
         ;; the BendSpanner
         (all-left-right-note-heads (get-bound-note-heads grob))
         (left-right-note-heads
           (cons
             (bend::remove-certain-tab-note-heads 
               (car all-left-right-note-heads))
             (bend::remove-certain-tab-note-heads 
               (cdr all-left-right-note-heads)))))
          
    (cond 
      ((or (null? (car left-right-note-heads))
           (null? (cdr left-right-note-heads)))
       (begin 
         (ly:warning "no notes to start a bend from found. If you want to  bend 
an open string, consider override/tweak like: TabNoteHead.bend-me = ##t")
         (ly:grob-suicide! grob)))
      ((not (integer? factor))
       (begin 
         (ly:warning 
           (format #f "factor ~a needs to be an integer value." factor))
         (ly:grob-suicide! grob)))
      (else (let* ((staff-symbol (ly:grob-object grob 'staff-symbol))
                   (line-count (ly:grob-property staff-symbol 'line-count))
                   (staff-space (ly:staff-symbol-staff-space grob))
                   (staff-radius (ly:staff-symbol-staff-radius grob))
                   (staff-symbol-line-thickness 
                     (ly:staff-symbol-line-thickness grob))
                   (style (ly:grob-property grob 'style))
                   (dashed-line-settings
                     (assoc-get 
                       'dashed-line-settings
                       (ly:grob-property grob 'details)))
                   (font-size (ly:grob-property grob 'font-size 0.0))
                   (scale-factor (magstep font-size))
                   (orig (ly:grob-original grob))
                   (siblings (if (ly:grob? orig)
                                 (ly:spanner-broken-into orig)
                                 '()))
                   ;; get the top-most TabNoteHeads
                   (top-right-tab-nh 
                     (get-top-most-tab-head 
                       (cdr all-left-right-note-heads)))
                   (top-left-tab-nh 
                     (get-top-most-tab-head 
                       (car all-left-right-note-heads)))
                   (sorted-left-right-pitches 
                     (get-pitches-from-bound-note-heads grob))
                   (quarter-diffs 
                     (get-quarter-diffs sorted-left-right-pitches))
                   (bend-direction
                     (cond ((negative? quarter-diffs) DOWN)
                           ((positive? quarter-diffs) UP)
                           ;; TODO any use case?
                           (else 0)))
                   (begin-x-list 
                     (bend::calc-bend-x-begin 
                       grob 
                       siblings 
                       left-right-note-heads 
                       factor 
                       quarter-diffs))
                   (begin-x (car begin-x-list))
                   (begin-y-list
                     (bend::calc-start-end-y-coords 
                       grob staff-space 
                       ;; for pre-bend and pre-bend-hold we take 
                       ;; all note-heads into account to avoid collisions
                       ;; with a probably not bended open string
                       (if (or (eq? style 'pre-bend) 
                               (eq? style 'pre-bend-hold))
                           (car all-left-right-note-heads)
                           (car left-right-note-heads))
                       quarter-diffs))
                   (curve-x-padding-line-end
                      (if (and (not (null? siblings)) 
                               (not (eq? style 'pre-bend-hold))
                               (equal? grob (car siblings)))
                          (assoc-get 
                            'curve-x-padding-line-end 
                            (ly:grob-property grob 'details) 
                            0)
                          0))
                   (end-x 
                     (bend::calc-bend-x-end 
                       grob siblings top-left-tab-nh top-right-tab-nh))
                   (y-distance-from-tabstaff-to-arrow-tip
                     (* scale-factor
                       (assoc-get 
                         'y-distance-from-tabstaff-to-arrow-tip
                         (ly:grob-property grob 'details))))
                   (end-y 
                     (lambda (mult) 
                       (+ 
                          (* (/ (- line-count 1) 2) staff-space) 
                          (* mult y-distance-from-tabstaff-to-arrow-tip))))
                   (bend-arrowhead-width 
                     (* scale-factor
                       (assoc-get 
                         'bend-arrowhead-width
                         (ly:grob-property grob 'details))))
                   (bend-arrowhead-height 
                     (* scale-factor
                       (assoc-get 
                         'bend-arrowhead-height
                         (ly:grob-property grob 'details))))
                   (bend-arrow-curvature-factor 
                     (assoc-get 
                       'bend-arrow-curvature-factor
                       (ly:grob-property grob 'details)))
                   (bend-line-thickness
                     (* staff-symbol-line-thickness 
                        (ly:grob-property grob 'thickness)))
                   (middle-x 
                     (+ 
                       begin-x 
                       (* bend-arrow-curvature-factor 
                          (- end-x begin-x))
                       ;; if the curve gets some padding at line-break
                       ;; do it here as well - warrants nice output
                       (- curve-x-padding-line-end)))
                   (arrow-stencil-proc
                     (assoc-get 
                       'arrow-stencil
                       (ly:grob-property grob 'details)))
                   (target-visibility 
                     (assoc-get 
                       'target-visibility
                       (ly:grob-property grob 'details)))
                   ;; A vector of 3 booleans, 
                   ;;    #(end-of-line unbroken begin-of-line)
                   (head-text-break-visibility
                     (assoc-get 
                       'head-text-break-visibility
                       (ly:grob-property grob 'details)))
                   (head-text-print-condition
                     (cond ((and (not (null? siblings)) 
                                 (equal? grob (car siblings)))
                            (vector-ref head-text-break-visibility 0))
                           ((and (not (null? siblings)) 
                                 (equal? grob (last siblings)))
                            (vector-ref head-text-break-visibility 2))
                           (else
                             (vector-ref head-text-break-visibility 1))))
                   (bend-amount 
                     ;; If 'text is not set, calculate it, 
                     ;; otherwise use its content
                     (if (null? (ly:grob-property grob 'text))
                         (bend::text-string grob)
                         (ly:grob-property grob 'text))))

              ;; for up-bends make target note heads transparent
              ;; down-bends will get the note-heads parenthesized via 
              ;; 'display-cautionary
              ;; for tied notes all notes except the ones from the first 
              ;; note-column become transparent
              (if (and (positive? quarter-diffs) (not target-visibility))
                  (let* ((covered-note-columns 
                           (ly:grob-array->list 
                             (ly:grob-object grob 'note-columns)))
                         (length-covered-note-columns
                           (length covered-note-columns)))
                  (make-tab-heads-transparent 
                    (if (>= length-covered-note-columns 2)
                        (append-map
                          (lambda (nc)
                            (ly:grob-array->list 
                              (ly:grob-object nc 'note-heads)))
                          (drop covered-note-columns 1))
                        (cdr left-right-note-heads)))))
                    
              ;; Hmmmm, needed to get correct skylines, but hmmm ...
              (ly:grob-set-property! grob 'Y-offset 0)
              
              ;; the final stencil
              (ly:stencil-add
                ;; text-stencil, indicating bend-amount
                ;; printed for up-bends only
                ;; in case of line-break it will be printed only at 
                ;; line-begin
                ;; apart from pre-bend and prebend-hold, where it is printed 
                ;; only at line end
                (if (and (> bend-direction -1)
                         head-text-print-condition)
                    ((bend::text-stencil 
                      (if (or (eq? style 'pre-bend) 
                              (eq? style 'pre-bend-hold))
                          begin-x
                          end-x)
                      (end-y factor) bend-amount) grob)
                    empty-stencil)
              (cond 
                ;; TODO Merge the settings for hold and (pre-)bend-hold
                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                ;; hold
                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                ((eq? style 'hold)
                   (let ((line-end-x-padding
                           (if (and (not (null? siblings))
                                    (equal? grob (car siblings)))
                               (assoc-get 
                                 'curve-x-padding-line-end 
                                 (ly:grob-property grob 'details) 
                                 0)
                               0)))
                     (bend::make-line-curve-stencil 
                       bend-line-thickness
                       (list
                         begin-x (end-y factor)
                         (- end-x line-end-x-padding) (end-y factor))
                       dashed-line-settings))
                 )
                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                ;; pre-bend and pre-bend-hold
                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                ((or (eq? style 'pre-bend) (eq? style 'pre-bend-hold))
                 (let* ((vertical-line
                          (if (or (null? siblings) 
                                  (equal? (car siblings) grob))
                              (bend::make-line-curve-stencil 
                                bend-line-thickness
                                (list
                                  begin-x (last begin-y-list)
                                  begin-x (end-y factor)))
                              empty-stencil))
                        (horizontal-line
                          (if (eq? style 'pre-bend-hold)
                              (let ((line-end-x-padding
                                      (if (and (not (null? siblings))
                                               (equal? grob (car siblings)))
                                          (assoc-get 
                                            'curve-x-padding-line-end 
                                            (ly:grob-property grob 'details) 
                                            0)
                                          0)))
                                (bend::make-line-curve-stencil 
                                  bend-line-thickness
                                  (list
                                    begin-x (end-y factor)
                                    (- end-x line-end-x-padding) (end-y factor))
                                  dashed-line-settings))
                              empty-stencil))
                         (arrow-head 
                          (if head-text-print-condition
                              (arrow-stencil-proc
                                 bend-line-thickness 
                                 ;;end-curve-coords:
                                 (cons 
                                   begin-x 
                                   (- (end-y factor) 
                                      bend-arrowhead-height))
                                 bend-arrowhead-height 
                                 bend-arrowhead-width
                                 bend-direction)
                              empty-stencil))
                              )
                   (ly:stencil-add 
                     arrow-head 
                     vertical-line
                     horizontal-line)))
                ;;;;;;;;;;;;;;;;;;;;;;
                ;; consecutive bends
                ;;;;;;;;;;;;;;;;;;;;;;
                (else      
                  (if (> factor 1)
                      (let* ((val (if (= bend-direction -1) 
                                      bend-direction 
                                      0))
                             (arrow-head
                               (if head-text-print-condition
                                   (arrow-stencil-proc 
                                      bend-line-thickness 
                                      ;; end-curve-coords
                                      (cons end-x 
                                            (+ (end-y (+ factor val)) 
                                               (* -1 
                                                  bend-direction 
                                                  bend-arrowhead-height)))
                                      bend-arrowhead-height 
                                      bend-arrowhead-width
                                      bend-direction)
                                   empty-stencil))
                             (curves
                               ((bend::draw-curves
                                   bend-line-thickness
                                   begin-x-list middle-x 
                                   end-x
                                   (list 
                                     (end-y 
                                       (+ factor 
                                          (* -1 bend-direction) 
                                          val)))
                                   (end-y (+ factor val))) 
                                   grob)))
                        (ly:stencil-add arrow-head curves ))
                      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                      ;;;; default bends, i.e. factor is 1
                      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                      (let* ((new-end-y
                               (if (> bend-direction -1)
                                   (end-y factor)
                                   (let* ((end-y-list
                                            (bend::calc-start-end-y-coords
                                              grob 
                                              staff-space 
                                              (cdr all-left-right-note-heads))))
                                     (last end-y-list))))
                             (new-begin-y
                               (if (> bend-direction -1)
                                   begin-y-list
                                   (list (end-y factor))))
                             (arrow
                               (if head-text-print-condition
                                   (arrow-stencil-proc
                                      bend-line-thickness 
                                      ;; end-curve-coords
                                      (cons 
                                        end-x 
                                        (+ new-end-y 
                                           (* -1 
                                              bend-direction 
                                              bend-arrowhead-height)))
                                      bend-arrowhead-height 
                                      bend-arrowhead-width
                                      bend-direction)
                                   empty-stencil))
                             (curve
                               ((bend::draw-curves
                                   bend-line-thickness
                                   begin-x-list 
                                   middle-x 
                                   end-x  
                                   new-begin-y
                                   new-end-y) 
                                   grob)))
                         (ly:stencil-add arrow curve)))))))))))