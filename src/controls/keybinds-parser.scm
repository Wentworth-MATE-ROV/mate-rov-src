#!/usr/bin/guile
!#
;;; Joe Jevnik
;;; 2014.3.13
;;; Parser of the .keybinds file.

;; Generates the button number in the form of a list of 'b and the number.
(define (button n)
  (list 'b (- n 1)))

;; Generates the axis number in the form of a list of 'a and the number or pair.
(define (axis n)
  (list 'a n))

;; Generates a pair that will represent an axis
(define (axis-pair a b)
  (axis (list a b)))

;; Lazily folds func over list
(define (list-and func list)
  (cond
   ((null? list) #t)
   ((func (car list)) (list-and func (cdr list)))
   (else #f)))

;; Are all the elements buttons?
(define (all-buttons? n)
  (list-and (lambda (m) (eq? 'b (car m))) n))

(define (all-axis? n)
    (list-and (lambda (m) (eq? 'a (car m))) n))

;; Functions that create the appropriate output of the defines.
(define (claw-open . n)
  (display (if (all-buttons? n) (append (list 'claw-open (length n)) n)
               "claw-open"))
  (newline))

(define (claw-close . n)
  (display (if (all-buttons? n) (append (list 'claw-close (length n)) n)
               "claw-close"))
  (newline))

(define (rotate-x . n)
  (display (if (all-axis? n) (append (list 'rotate-x (length n)) n)
               "rotate-x"))
  (newline))

(define (rotate-y . n)
  (display (if (all-axis? n) (append (list 'rotate-y (length n)) n)
               "rotate-y"))
  (newline))

(define (transpose . n)
  (display (if (all-axis? n) (append (list 'transpose (length n)) n)
               "transpose"))
  (newline))

(define (turn . n)
  (display (if (all-axis? n) (append (list 'turn (length n)) n)
               "turn"))
  (newline))

(define (thrust-mod . n)
  (display (if (all-axis? n) (append (list 'thrust-mod (length n)) n)
               "thrust-mod"))
  (newline))

;; Loads and evals argv[1], the config file.
(load (cadr (command-line)))
