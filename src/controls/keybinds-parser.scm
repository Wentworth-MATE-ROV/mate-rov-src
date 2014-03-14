#!/usr/bin/guile
!#
;;; Joe Jevnik
;;; 2014.3.13
;;; Parser of the .keybinds file.

;; Generates the button number in the form of a list of 'b and the number.
(define (button n)
  (list 'b (- n 1)))

(define no-button (button 0))

;; Generates the axis number in the form of a list of 'a and the number or pair.
(define (axis n)
  (list 'a n))

;; Generates a pair that will represent an axis
(define (axis-pair a b)
  (axis (list (- a 1) (- b 1))))

(define no-axis (axis -1))

;; Lazily folds func over list
(define (list-and func list)
  (cond
   ((null? list) #t)
   ((func (car list)) (list-and func (cdr list)))
   (else #f)))

;; Are all the elements buttons?
(define (all-buttons? n)
  (list-and (lambda (m) (eq? 'b (car m))) n))

;; Are all the elements axis?
(define (all-axis? n)
  (list-and (lambda (m) (eq? 'a (car m))) n))


;; Functions that create the appropriate output of the defines.
;; Buttons that open the claw.
(define (claw-open . n)
  (let ((l (length n)))
    (display (if (and (<= l 12) (all-buttons? n))
                 (append (list 'claw-open l) n)
                 "claw-open")))
  (newline))

;; Buttons that close the claw.
(define (claw-close . n)
  (let ((l (length n)))
    (display (if (and (<= l 12) (all-buttons? n))
                 (append (list 'claw-close l) n)
                 "claw-close")))
  (newline))

;; Axes that rotate the claw about the x axis.
(define (claw-x . n)
  (let ((l (length n)))
    (display (if (and (<= l 6) (all-axis? n))
                 (append (list 'claw-x l n))
                 "claw-x")))
  (newline))

;; Axes that rotate the claw about the y axis.
(define (claw-y . n)
  (let ((l (length n)))
    (display (if (and (<= l 6) (all-axis? n))
                 (append (list 'claw-y l n))
                 "claw-y")))
  (newline))

;; Axes that rotate ABOUT the y axis.
(define (rotate-y . n)
  (let ((l (length n)))
    (display (if (and (<= l 6) (all-axis? n))
                 (append (list 'rotate-y l n))
                 "rotate-y")))
  (newline))

;; Axes that rotate ABOUT the z axis.
(define (rotate-z . n)
  (let ((l (length n)))
    (display (if (and (<= l 6) (all-axis? n))
                 (append (list 'rotate-z l n))
                 "rotate-z")))
  (newline))

;; Axes that turn ABOUT the y axis.
(define (turn-y . n)
  (let ((l (length n)))
    (display (if (and (<= l 6) (all-axis? n))
                 (append (list 'turn-y l n))
                 "turn-y")))
  (newline))

;; Axes that transpose the robot in x direction.
(define (transpose-x . n)
  (let ((l (length n)))
    (display (if (and (<= l 6) (all-axis? n))
                 (append (list 'transpose-x l n))
                 "transpose-x")))
  (newline))

;; Axes that transpose the robot in the y direction.
(define (transpose-y . n)
  (let ((l (length n)))
    (display (if (and (<= l 6) (all-axis? n))
                 (append (list 'transpose-y l n))
                 "transpose-y")))
  (newline))

;; Axes that control the thrust modifier.
(define (thrust-mod . n)
  (let ((l (length n)))
    (display (if (and (<= l 6) (all-axis? n))
                 (append (list 'thrust-mod l n))
                 "thrust-mod")))
  (newline))

;; Buttons that turn the laser device on.
(define (laser-on . n)
  (let ((l (length n)))
    (display (if (and (<= l 12) (all-buttons? n))
                 (append (list 'laser-on l) n)
                 "laser-on")))
  (newline))

;; Buttons that turn the laser device off.
(define (laser-off . n)
  (let ((l (length n)))
    (display (if (and (<= l 12) (all-buttons? n))
                 (append (list 'laser-off l) n)
                 "laser-off")))
  (newline))

;; Buttons that toggle the state of the laser device.
(define (laser-toggle . n)
  (let ((l (length n)))
    (display (if (and (<= l 12) (all-buttons? n))
                 (append (list 'laser-toggle l) n)
                 "laser-toggle")))
  (newline))


;; Loads and evals argv[1], the config file.
(load (cadr (command-line)))
