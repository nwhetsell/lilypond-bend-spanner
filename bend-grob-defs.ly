\version "2.19.46"

#(define-event-class 'bend-span-event 'span-event)

#(define (add-grob-definition grob-name grob-entry)
   (let* ((meta-entry   (assoc-get 'meta grob-entry))
          (class        (assoc-get 'class meta-entry))
          (ifaces-entry (assoc-get 'interfaces meta-entry)))
     (set-object-property! grob-name 'translation-type? ly:grob-properties?)
     (set-object-property! grob-name 'is-grob? #t)
     (set! ifaces-entry (append (case class
                                  ((Item) '(item-interface))
                                  ((Spanner) '(spanner-interface))
                                  ((Paper_column) '((item-interface
                                                     paper-column-interface)))
                                  ((System) '((system-interface
                                               spanner-interface)))
                                  (else '(unknown-interface)))
                                ifaces-entry))
     (set! ifaces-entry (uniq-list (sort ifaces-entry symbol<?)))
     (set! ifaces-entry (cons 'grob-interface ifaces-entry))
     (set! meta-entry (assoc-set! meta-entry 'name grob-name))
     (set! meta-entry (assoc-set! meta-entry 'interfaces
                                  ifaces-entry))
     (set! grob-entry (assoc-set! grob-entry 'meta meta-entry))
     (set! all-grob-descriptions
           (cons (cons grob-name grob-entry)
                 all-grob-descriptions))))

#(define (define-grob-property symbol type? description)
  (if (not (equal? (object-property symbol 'backend-doc) #f))
      (ly:error (_ "symbol ~S redefined") symbol))

  (set-object-property! symbol 'backend-type? type?)
  (set-object-property! symbol 'backend-doc description)
  symbol)

#(for-each
  (lambda (x)
    (apply define-grob-property x))
    `((bend-me
      ,boolean?
      "DOCME")))

#(define bend-amount-strings-alist
  '((quarter . "¼")
    (half . "½")
    (three-quarter . "¾")
    (full . #f)))

#(define bend-details
  `((bend-line-thickness . 0.1)
    (bend-arrow-curvature-factor . 0.35)
    (bend-amount-strings . ,bend-amount-strings-alist)
    ;; A vector of 3 booleans, #(end-of-line unbroken begin-of-line)
    (head-text-break-visibility . #(#f #t #t))
    (y-distance-from-tabstaff-to-arrow-tip . 2.75)
    ;; TODO implement usage?
    ;(consecutive-bends-arrow-height . 2.75)
    (bend-arrowhead-height . 1.25)
    (bend-arrowhead-width . 0.8)
    (arrow-stencil . ,bend::arrow-head-stencil)
    (curve-x-padding-line-end . 0.5)
    (curve-y-padding-line-end . 1)
    (dashed-line-settings . (dashed 0.4 0.4 0));; (style on off phase)
    (vertical-padding . 0.2)
    (horizontal-left-padding . 0.1)
    (y-distance-from-staffline-to-arrow . 0.2)
    (target-visibility . #f)))

%% From IR:
%% ly:add-interface iface desc props
%%
%%     Add a new grob interface. iface is the interface name, desc is the
%%     interface description, and props is the list of user-settable properties
%%     for the interface.
%%

%% Of no use as long as the engraver is in TabVoice. Why not?
%% In score context it responds
#(ly:add-interface
  'bend-interface
  "The (curved) line representing a bend."
  '(;gap
    ;length-fraction
    ;maximum-gap
    ;note-heads
    ;thickness
    ))

#(define bend-spanner-poperties-alist
  `((avoid-slur . ignore)
    (baseline-skip . 3)
    (before-line-breaking . ,bend::target-cautionary)
    (cross-staff . #f)
    (details . ,bend-details)
    (direction . ,UP)
    (font-size . -2)
    (font-shape . italic)
    (font-encoding . latin1)
    (line-thickness . 0.8)
    ;; TODO need or want minimum-length?
    (minimum-length . 1.5)
    (padding . 0.15)
    ;; TODO need or want spanner-id?
    (spanner-id . "")
    ;; TODO need springs-and-rods?
    ;(springs-and-rods . ,ly:spanner::set-spacing-rods)
    (stencil . ,(bend::draw-bend-arrow-stencil))
    (style . default)
    (text . ,bend::text-string)
    (thickness . 1)
    (vertical-skylines . ,grob::unpure-vertical-skylines-from-stencil)
    (word-space . 0.6)
    ;; TODO what about Y-extent?
    ;(Y-extent . (-1 . 1))
    (meta . ((class . Spanner)
             (interfaces . (outside-staff-interface
                            font-interface
                            bend-interface
                            spanner-interface
                            line-spanner-interface
                            text-interface
                            text-script-interface))))))

#(add-grob-definition
  'BendSpanner bend-spanner-poperties-alist)

#(define bend-event-spanner-types
   '(
     (BendSpanEvent
      . ((description . "Used to signal where scheme text spanner brackets
start and stop.")
         (types . (bend-span-event post-event span-event event))
         ))
     ))

#(set!
  bend-event-spanner-types
  (map (lambda (x)
         (set-object-property! (car x)
                               'music-description
                               (cdr (assq 'description (cdr x))))
         (let ((lst (cdr x)))
           (set! lst (assoc-set! lst 'name (car x)))
           (set! lst (assq-remove! lst 'description))
           (hashq-set! music-name-to-property-table (car x) lst)
           (cons (car x) lst)))
       bend-event-spanner-types))

#(set! music-descriptions
       (append bend-event-spanner-types music-descriptions))

#(set! music-descriptions
       (sort music-descriptions alist<?))

#(define (add-bound-item context spanner item)
  (if (null? (ly:spanner-bound spanner LEFT))
      (ly:spanner-set-bound! spanner LEFT item)
      (ly:spanner-set-bound! spanner RIGHT item)))

#(define (axis-offset-symbol axis)
  (if (eq? axis X) 'X-offset 'Y-offset))

#(define (set-axis! grob axis)
  (if (not (number? (ly:grob-property grob 'side-axis)))
      (begin
        (set! (ly:grob-property grob 'side-axis) axis)
        (ly:grob-chain-callback
         grob
         (if (eq? axis X)
             ly:side-position-interface::x-aligned-side
             side-position-interface::y-aligned-side)
         (axis-offset-symbol axis)))))

BendMeEngraver =
#(lambda (context)
  (let ((string-numbers '())
        (fingerings '())
        (note-events '())
        (ties '())
        (tab-note-heads '()))
    (make-engraver
      ((start-translation-timestep trans)
        (set! note-events '())
        (set! tab-note-heads '())
        (set! fingerings '())
        (set! string-numbers '()))
      (listeners
        ((note-event engraver event)
          (set! note-events (cons event note-events)))
        ((string-number-event engraver event)
          (set! string-numbers (cons event string-numbers)))
        ((fingering-event engraver event)
          (set! fingerings (cons event fingerings))))
      (acknowledgers
       ((tab-note-head-interface engraver grob source-engraver)
        (set! tab-note-heads (cons grob tab-note-heads)))
       ((tie-interface engraver grob source-engraver)
        (set! ties (cons grob ties))))
      ((stop-translation-timestep trans)
        (let* ((arts
                 (map
                   (lambda (nv) (ly:event-property nv 'articulations))
                   note-events))
               (strings-from-articulations
                 (map
                   (lambda (a)
                     (filter
                       (lambda (x)
                         (member
                           'string-number-event
                           (ly:event-property x 'class)))
                       a))
                   arts))
                (strings
                   (if (every null? strings-from-articulations)
                       string-numbers
                       strings-from-articulations))
                (strings-to-determine-frets
                  (if (null? strings)
                      (list (make-list (length note-events) '()))
                      strings))
                (fingers-from-articulations
                  (map
                    (lambda (a)
                     (filter
                      (lambda (x)
                        (member 'fingering-event (ly:event-property x 'class)))
                      a))
                    arts))
                (fingers
                  (if (every null? fingers-from-articulations)
                      fingerings
                      fingers-from-articulations))
                (fingers-to-determine-frets
                  (if (null? fingers)
                      (list (make-list (length note-events) '()))
                      fingers))
                (unnest-list-one-level
                  (lambda (ls)
                    (append-map (lambda (x) (if (pair? x) x (list x))) ls)))
                (list-for-determine-frets
                    (list
                      (unnest-list-one-level strings-to-determine-frets)
                      (unnest-list-one-level fingers-to-determine-frets)))
                (string-fret-fingers
                  ((ly:context-property context 'noteToFretFunction)
                    context
                    note-events
                    list-for-determine-frets)))

          ;; exclude tied notes from being bent, if Tie.bend-me is set #f
          (if (not (null? ties))
              (for-each
                (lambda (tie)
                  (let ((tie-bounds
                          (list
	                        (ly:spanner-bound tie LEFT)
	                        (ly:spanner-bound tie RIGHT))))
	                (for-each
	                  (lambda (bound)
	                    (if (and (ly:grob? bound)
	                             (not (ly:grob-property tie 'bend-me)))
	                        (ly:grob-set-property! bound 'bend-me #f)))
	                  tie-bounds)))
	            ties))

	      ;; exlude open strings from being bent,
	      ;; if TabNoteHead.bend-me is unset, i.e. '()
          (for-each
            (lambda (tnh strg-frt-fngr)
              (if (and (list? strg-frt-fngr)
                       (number? (cadr strg-frt-fngr))
                       (zero? (cadr strg-frt-fngr))
                       (null? (ly:grob-property tnh 'bend-me)))
                  (ly:grob-set-property! tnh 'bend-me #f)))
            tab-note-heads
            string-fret-fingers)))
      ((finalize trans)
       (set! ties '())))))

BendSpannerEngraver =
#(lambda (context)
  (let ((span '())
        (finished '())
        (event-start '())
        (event-stop '()))
    (make-engraver
      (listeners
         ((bend-span-event engraver event)
           (if (= START (ly:event-property event 'span-direction))
               (set! event-start event)
               (set! event-stop event))))
      (acknowledgers ((note-column-interface engraver grob source-engraver)
                      (if (ly:spanner? span)
                          (begin
                            (ly:pointer-group-interface::add-grob
                              span 'note-columns grob)
                            (add-bound-item context span grob)))
                      (if (ly:spanner? finished)
                          (begin
                            (ly:pointer-group-interface::add-grob
                              finished 'note-columns grob)
                            (add-bound-item context finished grob))))
                     ((bend-interface engraver grob source-engraver)
                        (write-me
                          "grob seen in BendSpannerEngraver-acknowledgers"
                          grob))
                            ) ;; end acknowledgers
      ((process-music trans)
       (if (ly:stream-event? event-stop)
           (if (null? span)
               (ly:warning "You're trying to end a scheme text spanner but you
haven't started one.")
               (begin (set! finished span)
                      (ly:engraver-announce-end-grob trans finished event-start)
                      (set! span '())
                      (set! event-stop '()))))
       (if (ly:stream-event? event-start)
           (begin (set! span
                       (ly:engraver-make-grob trans 'BendSpanner event-start))
                  (set-axis! span Y)
                  (set! event-start '()))))
      ((stop-translation-timestep trans)
       (if (and (ly:spanner? span)
                (null? (ly:spanner-bound span LEFT)))
           (ly:spanner-set-bound! span LEFT
             (ly:context-property context 'currentMusicalColumn)))
       (if (ly:spanner? finished)
           (begin
             (if (null? (ly:spanner-bound finished RIGHT))
                 (ly:spanner-set-bound! finished RIGHT
                   (ly:context-property context 'currentMusicalColumn)))
             (set! finished '())
             (set! event-start '())
             (set! event-stop '()))))
      ((finalize trans)
       (if (ly:spanner? finished)
           (begin
             (if (null? (ly:spanner-bound finished RIGHT))
                 (ly:spanner-set-bound! finished RIGHT
                   (ly:context-property context 'currentMusicalColumn)))
             (set! finished '())))
       (if (ly:spanner? span)
           (begin
             (ly:warning "I think there's a dangling bend spanner :-(
\t maybe \\stopBend not set?")
             (ly:grob-suicide! span)
             (set! span '())))))))

startBend =
#(make-span-event 'BendSpanEvent START)

stopBend =
#(make-span-event 'BendSpanEvent STOP)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% \layout for the BendSpanner
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\layout {
  \context {
    \Global
    \grobdescriptions #all-grob-descriptions
  }
  \context {
    \Score
    supportNonIntegerFret = ##t
  }
  \context {
    \Voice
    %% Currently no separate stencil for use in Voice defined
    %\consists \BendSpannerEngraver
  }
  \context {
    \TabVoice
    \consists \BendSpannerEngraver
    \consists \BendMeEngraver
  }
}