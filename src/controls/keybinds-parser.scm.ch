// This file was generated by fch.
// To submit a bug report or add a language module, go to:
// <https://github.com/llllllllll/fch>
#ifndef ROV_KEYBINDS_PARSER_H
#define ROV_KEYBINDS_PARSER_H
const char* const keybinds_parser_str = "#| keybinds-parser.scm --- Parser for the .keybinds config file.\n   Copyright (c) Joe Jevnik\n\n   This program is free software; you can redistribute it and/or modify it\n   under the terms of the GNU General Public License as published by the Free\n   Software Foundation; either version 3 of the License, or (at your option)\n   any later version.\n\n   This program is distributed in the hope that it will be useful, but WITHOUT\n   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or\n   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for\n   more details.\n\n   You should have received a copy of the GNU General Public License along with\n   this program; if not, write to the Free Software Foundation, Inc., 51\n   Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. |#\n\n(define-module (parsers keybinds-parser))\n\n;; Generates the button number in the form of a list of 'b and the number.\n;; n -- the number of the button on the joystick: (button 2) is button 2.\n(define-public (button n)\n  (if (or (> n 12) (< n 1))\n      -1\n      (- n 1)))\n\n;; Defines a constant for no button, expanding to (b -1) or 255 on the c end.\n(define-public no-button (button 0))\n\n;; Generates the axis number in the form of a list of 'a and the number or pair.\n;; n -- the axis number.\n(define-public (axis n)\n  (if (or (> n 6) (< n 0))\n      -1\n      n))\n\n;; Generates a pair that will represent an axis.\n(define-public (axis-pair a b)\n  (if (or (eq? no-button a) (eq? no-button b))\n      no-axis\n      (cons a b)))\n\n(define no-axis (axis -1))\n\n;; Lazily folds a function over list.\n;; f -- the function to fold.\n;; ns -- the list to fold over.\n(define (list-and f ns)\n  (cond\n   ((null? ns) #t)\n   ((f (car ns)) (list-and f (cdr ns)))\n   (else #f)))\n\n;; Are all the elements buttons?\n;; n -- the list to check.\n(define (all-buttons? n)\n  (>= 12 (length n)))\n\n;; Are all the elements axis?\n;; n -- the list to check.\n(define (all-axis? n)\n  (>= 6 (length n)))\n\n\n;; Create a handler for an operation.\n;; name -- the name of the operation.\n;; p -- the predicate used to test the list of arguments.\n(define-syntax gen-op\n  (syntax-rules ()\n    ((gen-op name p)\n     (define-public (name . n)\n       (if (p n)\n           `(,(symbol->string 'name) . ,n)\n           'name)))))\n\n;; Generate the operations.\n(gen-op claw-open        all-buttons?)\n(gen-op claw-close       all-buttons?)\n(gen-op claw-x           all-axis?)\n(gen-op claw-y           all-axis?)\n(gen-op rotate-y         all-axis?)\n(gen-op rotate-z         all-axis?)\n(gen-op turn-y           all-axis?)\n(gen-op transpose-x      all-axis?)\n(gen-op transpose-y      all-axis?)\n(gen-op laser-toggle     all-buttons?)\n(gen-op sidelight-toggle all-buttons?)\n(gen-op headlight-toggle all-buttons?)\n";
const size_t keybinds_parser_len = 2768;
#endif