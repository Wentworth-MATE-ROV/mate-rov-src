#| logic.scm --- Parser of the pin-parser.scm
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

;; SHRT_MAX as defined in limits.h on x86_64 Linux 3.14.2-1-ARCH.
(define shrt-max 32767)

;; SHRT_MIN as defined in limits.h on x86_64 Linux 3.14.2-1-ARCH.
(define shrt-min -32768)

;; Scales the values from a joystick to what the ctrl-state wants.
;; return: An integer
(define (scale-axis-value v)
  (round (* 100 (/ v shrt-max))))

;; Truncates the sum of two axis values
(define (trunc-sum-axis-values v w)
  (let ((s (+ v w)))
    (cond ((> s shrt-max) shrt-max)
          ((< s shrt-min) shrt-min)
          (else s))))

;; Scales and returns the power to send to the left motor.
(define (scale-left-motor input-state)
  (let ((trans-x (transpose-x input-state))
        (rot-y   (rotate-y    input-state)))
    (scale-axis-value (trunc-sum-axis-values trans-x rot-y))))

;; Scales and returns to the power to send to the right motor
(define (scale-right-motor input-state)
  (let ((trans-x (transpose-x input-state))
        (rot-y   (rotate-y    input-state)))
    (scale-axis-value (trunc-sum-axis-values trans-x (- 0 rot-y)))))

;; Scales and returns the power to send to the front motor.
(define (scale-front-motor input-state)
  (let ((trans-y (transpose-y input-state))
        (rot-z   (rotate-z    input-state)))
    (scale-axis-value (trunc-sum-axis-values trans-y rot-z))))

;; Scales and returns the power to send to the back motor.
(define (scale-back-motor input-state)
  (let ((trans-y (transpose-y input-state))
        (rot-z   (rotate-z    input-state)))
    (scale-axis-value (trunc-sum-axis-values trans-y (- 0 rot-z)))))

;; (claw-90 . claw-180)
(define (get-piston-state claw-x claw-y old-state)
  (cond ((and (< claw-y 0) (= claw-x 0)) '(#t . #f))    ; Straight up
        ((and (> claw-y 0) (= claw-x 0)) '(#t . #t))    ; Straight down.
        ((and (< claw-x 0) (= claw-y 0)) '(#f . #t))    ; Straight foward.
        ((and (> claw-x 0) (= claw-y 0)) '(#f . #f))    ; Straight Back.
        (else old-state)))

;; Setup control state; populate extra if you wish.
(define (initialize ctrl-state)
  ctrl-state)

;; The main logic step.
;; Invoked with the paramaters:
;;   input-state: The state of the joystick this frame.
;;   delta-t:     The time in milliseconds since the last frame.
;;   ctrl-state:  The <ctrl-state> returned from the last frame.
;; On the first frame, delta-t will be a negative value and
;; the ctrl-state will be a default (zeroed) <ctrl-state>.
(define (logic-step input-state delta-t ctrl-state)
  (let* ((old-piston-state (cons (claw-90 ctrl-state) (claw-180 ctrl-state)))
         (new-piston-state (get-piston-state (claw-x input-state)
                                             (claw-y input-state)
                                             old-piston-state))
         (claw-90-state (car new-piston-state))
         (claw-180-state (cdr new-piston-state)))
    (mk-ctrl-state
     (scale-left-motor  input-state)
     (scale-right-motor input-state)
     (scale-front-motor input-state)
     (scale-back-motor  input-state)
     (if (headlight-toggle input-state)
         (not (headlights ctrl-state))
         (headlights ctrl-state))
     (if (sidelight-toggle input-state)
         (not (sidelights ctrl-state))
         (sidelights ctrl-state))
     (if (laser-toggle input-state)
         (not (lasers ctrl-state))
         (lasers ctrl-state))
     (cond ((claw-close input-state) #t)
           ((claw-open  input-state) #f)
           (else (claw-grip ctrl-state)))
     claw-90-state
     claw-180-state
     'e)))
