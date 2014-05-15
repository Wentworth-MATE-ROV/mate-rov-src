#!/usr/bin/guile
!#
;;; Joe Jevnik
;;; 2014.3.13
;;; Parser of the .keybinds file.

;; Generates the button number in the form of a list of 'b and the number.
;; n -- the number of the button on the joystick: (button 2) is button 2.
(define (button n)
  (if (or (> n 12) (< n 1))
      -1
      (- n 1)))

;; Defines a constant for no button, expanding to (b -1) or 255 on the c end.
(define no-button (button 0))

;; Generates the axis number in the form of a list of 'a and the number or pair.
;; n -- the axis number.
(define (axis n)
  (if (or (> n 6) (< n 0))
      -1
      n))

;; Generates a pair that will represent an axis.
(define (axis-pair a b)
  (if (or (eq? no-button a) (eq? no-button b))
      no-axis
      (cons a b)))

(define no-axis (axis -1))

;; Lazily folds a function over list.
;; f -- the function to fold.
;; ns -- the list to fold over.
(define (list-and f ns)
  (cond
   ((null? ns) #t)
   ((f (car ns)) (list-and f (cdr ns)))
   (else #f)))

;; Are all the elements buttons?
;; n -- the list to check.
(define (all-buttons? n)
  (>= 12 (length n)))

;; Are all the elements axis?
;; n -- the list to check.
(define (all-axis? n)
  (>= 6 (length n)))


;; Create a handler for an operation.
;; name -- the name of the operation.
;; p -- the predicate used to test the list of arguments.
(define-syntax gen-op
  (syntax-rules ()
    ((gen-op name p)
     (define (name . n)
       (if (p n)
           `(name ,(length n) ,n)
           'name)))))

;; Generate the operations.
(gen-op claw-open        all-buttons?)
(gen-op claw-close       all-buttons?)
(gen-op claw-x           all-axis?)
(gen-op claw-y           all-axis?)
(gen-op rotate-y         all-axis?)
(gen-op rotate-z         all-axis?)
(gen-op turn-y           all-axis?)
(gen-op transpose-x      all-axis?)
(gen-op transpose-y      all-axis?)
(gen-op laser-toggle     all-buttons?)
(gen-op sidelight-toggle all-buttons?)
(gen-op headlight-toggle all-buttons?)

;; Loads and evals argv[1], the config file.
;(load (cadr (command-line)))
