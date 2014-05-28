#| keybinds-parser.scm --- Parser for the .keybinds config file.
   Copyright (c) Joe Jevnik

   This program is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 3 of the License, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   You should have received a copy of the GNU General Public License along with
   this program; if not, write to the Free Software Foundation, Inc., 51
   Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. |#

(define-module (parsers keybinds-parser))

;; Generates the button number in the form of a list of 'b and the number.
;; n -- the number of the button on the joystick: (button 2) is button 2.
(define-public (button n)
  (if (or (> n 12) (< n 1))
      -1
      (- n 1)))

;; Defines a constant for no button, expanding to (b -1) or 255 on the c end.
(define-public no-button (button 0))

;; Generates the axis number in the form of a list of 'a and the number or pair.
;; n -- the axis number.
(define-public (axis n)
  (if (or (> n 6) (< n 0))
      -1
      n))

;; Generates a pair that will represent an axis.
(define-public (axis-pair a b)
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
     (define-public (name . n)
       (if (p n)
           `(,(symbol->string 'name) . ,n)
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
