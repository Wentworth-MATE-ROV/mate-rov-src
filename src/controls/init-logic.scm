(use-modules (srfi srfi-9 gnu))

;; The robot control state.
(define-immutable-record-type <ctrl-state>
  (mk-ctrl-state left-motor right-motor front-motor back-motor
                 headlights sidelights lasers claw-grip extra)
  ctrl-state?
  (left-motor left-motor set-left-motor)
  (right-motor right-motor set-right-motor)
  (front-motor front-motor set-front-motor)
  (back-motor back-motor set-back-motor)
  (headlights headlights set-headlights)
  (sidelights sidelights set-sidelights)
  (lasers lasers set-lasers)
  (claw-grip claw-grip set-claw-grip)
  (extra extra set-extra))

;; The cleaned joystick.
(define-immutable-record-type <js-state>
  (mk-js-state claw-open claw-close laser-toggle headlight-toggle
               sidelight-toggle claw-x claw-y rotate-z rotate-y
               transpose-x transpose-y)
  js-state?
  (claw-open claw-open set-claw-open)
  (claw-close claw-close set-claw-close)
  (laser-toggle laser-toggle set-laser-toggle)
  (headlight-toggle headlight-toggle set-headlight-toggle)
  (sidelight-toggle sidelight-toggle set-sidelight-toggle)
  (claw-x claw-x set-claw-x)
  (claw-y claw-y set-claw-y)
  (rotate-z rotate-z set-rotate-z)
  (rotate-y rotate-y set-rotate-y)
  (transpose-x transpose-x set-transpose-x)
  (transpose-y transpose-y set-transpose-y))


;; Checks the types of the control state and sets default values.
(define (sanatize-ctrl-state ctrl)
  (letrec ((check-field (lambda (c pred getter setter v)
                          (if (pred (getter c))
                              c
                              (setter c v))))
           (check-fields (lambda (init forms)
                           (if (null? forms)
                               init
                               (check-fields
                                (apply check-field
                                       (cons init (car forms)))
                                (cdr forms))))))
    (check-fields ctrl `((,integer? ,front-motor ,set-front-motor 0)
                         (,integer? ,back-motor ,set-back-motor 0)
                         (,integer? ,left-motor ,set-left-motor 0)
                         (,integer? ,right-motor ,set-right-motor 0)
                         (,boolean? ,headlights ,set-headlights #f)
                         (,boolean? ,sidelights ,set-sidelights #f)
                         (,boolean? ,lasers ,set-lasers #f)
                         (,boolean? ,claw-grip ,set-claw-grip #f)))))\