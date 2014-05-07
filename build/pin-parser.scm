#!/usr/bin/guile
!#
;;; Joe Jevnik
;;; 2014.4.21
;;; Parser of the .pins file.

;; Lazily folds a function over list.
;; f -- the function to fold.
;; ns -- the list to fold over.
(define (list-and f ns)
  (cond
   ((null? ns) #t)
   ((f (car ns)) (list-and f (cdr ns)))
   (else #f)))

;; Defines a new operation-pin pair.
(define-syntax gen-pin
  (syntax-rules ()
    ((gen-pin name)
     (define (name ps)
       (display (if (list-and (lambda (p) (or (> p 54) (< p 0))) ps)
                    'name
                    `(name ,(length ps) ,ps)))
       (newline)))))

(gen-pin lasers)
(gen-pin headlights)
(gen-pin sidelights)
(gen-pin claw-grip)
(gen-pin left-motor)
(gen-pin left-motor-direction)
(gen-pin left-motor-ssg)
(gen-pin right-motor)
(gen-pin right-motor-direction)
(gen-pin right-motor-ssg)
(gen-pin front-motor)
(gen-pin front-motor-direction)
(gen-pin front-motor-ssg)
(gen-pin back-motor)
(gen-pin back-motor-direction)
(gen-pin back-motor-ssg)

;; Loads and evals argv[1], the config file.
(load (cadr (command-line)))
