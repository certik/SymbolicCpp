

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


; differ_eg.lisp

(load "differ.lisp")
(load "simp3.lisp")

(defun show1 (a)
   `(format t "~A~%  => ~A~%" (quote ,a) ,a))

(defmacro show (&rest a)
   `(progn ,@(mapcar #'show1 a)))

; Applications
(show
  ;simple-examples
  (diff 'x 'x)
  (diff '2 'x)
  (diff 2 'x)
  (diff 'x 'u)

  ;sum-rule
  (diff '(+ x x) 'x)

  ;product-rule
  (diff '(* x x) 'x)
  (simp (diff '(* x x) 'x))

  ;more-examples
  (diff '(* (+ 3 x) (- a x)) 'x)
  (simp (diff '(* (+ 3 x) (- a x)) 'x))
  (diff '(* (* x x) x) 'x)
  (simp (diff '(* (* x x) x) 'x))

  ;chain-rule
  (simp (diff '(cos (exp x)) 'x))
  (simp (diff '(/ (sin x) (cos x)) 'x))
  (simp (diff '(f (* (cos x) (exp x))) 'x))
)
