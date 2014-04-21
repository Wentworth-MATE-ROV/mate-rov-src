#!/usr/bin/guile
!#
;;; Joe Jevnik
;;; 2014.4.21
;;; Parser of the .pins file.

;; Defines a new operation-pin pair.
(define-syntax gen-pin
  (syntax-rules ()
    ((gen-pin name)
     (define (name p)
       (display (if (or (> p 54) (< p 0))
                    'name
                    `(name ,p)))
       (newline)))))

(gen-pin lasers)
(gen-pin headlights)
(gen-pin sidelights)

;; Loads and evals argv[1], the config file.
(load (cadr (command-line)))
