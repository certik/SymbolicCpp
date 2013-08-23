

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


; simp1_eg.lisp

(load "simp1.lisp")

(defun show1 (a)
   `(format t "~A~%  => ~A~%" (quote ,a) ,a))

(defmacro show (&rest a)
   `(progn ,@(mapcar #'show1 a)))


; Applications of the simplification

(show
  (simp-unary '(+ x))
  (simp-unary '(exp 0))
  (simp-unary '(log 1))
  (simp-unary '(cosh 0))
  (simp-unary '(exp 1))
  (simp-unary '(cos pi))
  (simp-unary '(- x))
  (simp-unary '(- 0))

  ; the following expressions are not simplified
  (simp-unary '(+ (+ x)))
  (simp-unary '(- (+ x)))
  (simp-unary '(+ (exp 0)))
  (simp-unary '(exp (sin 0)))
  (simp-unary '(log (exp (sin 0))))

  ; the last expression simplifies incorrectly
  (simp-unary '(+ x y))   ;                <--- WRONG ! 

  ; the following expressions generate errors
  (simp-unary 'x))   ; An attempt was made to do car on `x',
                     ; which is not a pair <--- ERROR !

  (simp-unary '(x))  ; An attempt was made to do car on `nil',
                     ; which is not a pair <--- ERROR !
)
