

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


; simp2_eg.lisp

(load "simp2.lisp")

(defun show1 (a)
   `(format t "~A~%  => ~A~%" (quote ,a) ,a))

(defmacro show (&rest a)
   `(progn ,@(mapcar #'show1 a)))


; Applications of the simplification
(show
  (simp 'x)
  (simp '(x))

  (simp '(+ x))
  (simp '(+ (+ x)))
  (simp '(+ (- x)))
  (simp '(+ (- 0)))
  (simp '(- (+ x)))

  (simp '(- (sin 0)))
  (simp '(+ (exp 0)))
  (simp '(- (exp 0)))
  (simp '(+ (log 1)))
  (simp '(+ (cosh 0)))
  (simp '(exp (sin 0)))
  (simp '(log (exp (sin 0))))

  (simp '(- (- x)))
  (simp '(+ x y))
)
