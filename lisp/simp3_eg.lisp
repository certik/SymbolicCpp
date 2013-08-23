

;;    SymbolicC++ : An object oriented computer algebra system written in C++
;;
;;    Copyright (C) 2008 Yorick Hardy and Willi-Hans Steeb
;;
;;    This program is free software; you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation; either version 2 of the License, or
;;    (at your option) any later version.
;;
;;    This program is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    GNU General Public License for more details.
;;
;;    You should have received a copy of the GNU General Public License along
;;    with this program; if not, write to the Free Software Foundation, Inc.,
;;    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.


; simp3_eg.lisp

(load "simp3.lisp")

(defun show1 (a)
   `(format t "~A~%  => ~A~%" (quote ,a) ,a))

(defmacro show (&rest a)
   `(progn ,@(mapcar #'show1 a)))


; Applications of the simplification

(show
  (simp '(+ (+ x)))
  (simp '(+ (- x)))
  (simp '(- (- x)))            ; not simplified !

  (simp '(+ (sin 0)))
  (simp '(+ (exp 0)))
  (simp '(+ (log 1)))
  (simp '(- (cosh 0)))
  (simp '(- (sin 0)))
  (simp '(- (arctan 0)))

  (simp '(+ x 0))
  (simp '(+ 0 x))
  (simp '(- x 0))
  (simp '(* x 0))
  (simp '(/ x 0))
  (simp '(/ 0 x))
  (simp '(- (/ x 0)))
  (simp '(power x 0))

  (simp '(+ x y))
  (simp '(+ (* x 0) (* x 1)))
  (simp '(- (+ 0 x) (* 1 x)))

  (simp '(- (+ x y) (- y x)))  ; not simplified !
)
