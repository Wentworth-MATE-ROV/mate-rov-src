#| init-logic.scm --- The required data types and functions needed to evaluate
                      the user's logic.scm
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

(use-modules (srfi srfi-9 gnu)
             (ice-9 format))

;; The robot control state.
(define-immutable-record-type <ctrl-state>
  (mk-ctrl-state left-motor right-motor front-motor back-motor
                 headlights sidelights lasers claw-grip claw-90 claw-180 extra)
  ctrl-state?
  (left-motor  left-motor  set-left-motor)
  (right-motor right-motor set-right-motor)
  (front-motor front-motor set-front-motor)
  (back-motor  back-motor  set-back-motor)
  (headlights  headlights  set-headlights)
  (sidelights  sidelights  set-sidelights)
  (lasers      lasers      set-lasers)
  (claw-grip   claw-grip   set-claw-grip)
  (claw-90     claw-90     set-claw-90)
  (claw-180    claw-180    set-claw-180)
  (extra       extra       set-extra))

;; The cleaned joystick and any sensors.
(define-immutable-record-type <input-state>
  (mk-input-state claw-open claw-close laser-toggle headlight-toggle
                  sidelight-toggle claw-x claw-y rotate-z rotate-y
                  transpose-x transpose-y)
  input-state?
  (claw-open        claw-open        set-claw-open)
  (claw-close       claw-close       set-claw-close)
  (laser-toggle     laser-toggle     set-laser-toggle)
  (headlight-toggle headlight-toggle set-headlight-toggle)
  (sidelight-toggle sidelight-toggle set-sidelight-toggle)
  (claw-x           claw-x           set-claw-x)
  (claw-y           claw-y           set-claw-y)
  (rotate-z         rotate-z         set-rotate-z)
  (rotate-y         rotate-y         set-rotate-y)
  (transpose-x      transpose-x      set-transpose-x)
  (transpose-y      transpose-y      set-transpose-y))


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
                         (,integer? ,back-motor  ,set-back-motor 0)
                         (,integer? ,left-motor  ,set-left-motor 0)
                         (,integer? ,right-motor ,set-right-motor 0)
                         (,boolean? ,headlights  ,set-headlights #f)
                         (,boolean? ,sidelights  ,set-sidelights #f)
                         (,boolean? ,lasers      ,set-lasers #f)
                         (,boolean? ,claw-grip   ,set-claw-grip #f)
                         (,boolean? ,claw-90     ,set-claw-90)
                         (,boolean? ,claw-180    ,set-claw-180)))))

;; Checks if a process exists or not.
(define (proc-exists? proc-name)
  (catch #t
    (eval-string (format #f "(lambda () ~a #t)" proc-name))
    (lambda (k . p) #f)))
