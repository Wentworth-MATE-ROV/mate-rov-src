// This file was generated by fch.
// To submit a bug report or add a language module, go to:
// <https://github.com/llllllllll/fch>
#ifndef ROV_INIT_LOGIC_H
#define ROV_INIT_LOGIC_H
const char* const init_logic_str = "#| init-logic.scm --- The required data types and functions needed to evaluate\n                      the user's logic.scm\n   Copyright (c) Joe Jevnik\n\n   This program is free software; you can redistribute it and/or modify it\n   under the terms of the GNU General Public License as published by the Free\n   Software Foundation; either version 3 of the License, or (at your option)\n   any later version.\n\n   This program is distributed in the hope that it will be useful, but WITHOUT\n   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or\n   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for\n   more details.\n\n   You should have received a copy of the GNU General Public License along with\n   this program; if not, write to the Free Software Foundation, Inc., 51\n   Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. |#\n\n(use-modules (srfi srfi-9 gnu)\n             (ice-9 format))\n\n;; The robot control state.\n(define-immutable-record-type <ctrl-state>\n  (mk-ctrl-state left-motor right-motor front-motor back-motor\n                 headlights sidelights lasers claw-grip claw-90 claw-180 extra)\n  ctrl-state?\n  (left-motor  left-motor  set-left-motor)\n  (right-motor right-motor set-right-motor)\n  (front-motor front-motor set-front-motor)\n  (back-motor  back-motor  set-back-motor)\n  (headlights  headlights  set-headlights)\n  (sidelights  sidelights  set-sidelights)\n  (lasers      lasers      set-lasers)\n  (claw-grip   claw-grip   set-claw-grip)\n  (claw-90     claw-90     set-claw-90)\n  (claw-180    claw-180    set-claw-180)\n  (extra       extra       set-extra))\n\n;; The cleaned joystick and any sensors.\n(define-immutable-record-type <input-state>\n  (mk-input-state claw-open claw-close laser-toggle headlight-toggle\n                  sidelight-toggle claw-x claw-y rotate-z rotate-y\n                  transpose-x transpose-y)\n  input-state?\n  (claw-open        claw-open        set-claw-open)\n  (claw-close       claw-close       set-claw-close)\n  (laser-toggle     laser-toggle     set-laser-toggle)\n  (headlight-toggle headlight-toggle set-headlight-toggle)\n  (sidelight-toggle sidelight-toggle set-sidelight-toggle)\n  (claw-x           claw-x           set-claw-x)\n  (claw-y           claw-y           set-claw-y)\n  (rotate-z         rotate-z         set-rotate-z)\n  (rotate-y         rotate-y         set-rotate-y)\n  (transpose-x      transpose-x      set-transpose-x)\n  (transpose-y      transpose-y      set-transpose-y))\n\n\n;; Checks the types of the control state and sets default values.\n(define (sanatize-ctrl-state ctrl)\n  (letrec ((check-field (lambda (c pred getter setter v)\n                          (if (pred (getter c))\n                              c\n                              (setter c v))))\n           (check-fields (lambda (init forms)\n                           (if (null? forms)\n                               init\n                               (check-fields\n                                (apply check-field\n                                       (cons init (car forms)))\n                                (cdr forms))))))\n    (check-fields ctrl `((,integer? ,front-motor ,set-front-motor 0)\n                         (,integer? ,back-motor  ,set-back-motor 0)\n                         (,integer? ,left-motor  ,set-left-motor 0)\n                         (,integer? ,right-motor ,set-right-motor 0)\n                         (,boolean? ,headlights  ,set-headlights #f)\n                         (,boolean? ,sidelights  ,set-sidelights #f)\n                         (,boolean? ,lasers      ,set-lasers #f)\n                         (,boolean? ,claw-grip   ,set-claw-grip #f)\n                         (,boolean? ,claw-90     ,set-claw-90)\n                         (,boolean? ,claw-180    ,set-claw-180)))))\n\n;; Checks if a process exists or not.\n(define (proc-exists? proc-name)\n  (catch #t\n    (eval-string (format #f \"(lambda () ~a #t)\" proc-name))\n    (lambda (k . p) #f)))\n";
const size_t init_logic_len = 3954;
#endif
