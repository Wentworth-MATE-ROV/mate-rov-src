#| pin-parser.scm --- Parser of the pin-parser.scm
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

(define-module (parsers pin-parser))

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
     (define-public (name ps)
       (if (list-and (lambda (p) (or (> p 54) (< p 0))) ps)
           'name
           `(,(symbol->string 'name) . ,ps))))))

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
