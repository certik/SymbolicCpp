

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


; poly_eg.lisp

(load "poly.lisp")

(defun show1 (a)
   `(format t "~A~%  => ~A~%" (quote ,a) ,a))

(defmacro show (&rest a)
   `(progn ,@(mapcar #'show1 a)))

(show
  (add '((2 . 3) (1 . 7) (0 . 1)) '((2 . 4) (0 . 2)))
  (add '((0 . 0)) '((3 . 4) (1 . 2) (0 . 7)))
  (negate '((2 . 4) (0 . 2)))
  (subtract '((2 . 3) (1 . 7) (0 . 1)) '((2 . 4) (0 . 2)))
  (multiply '((3 . 3) (0 . 2)) '((2 . 4) (1 . 1)))
  (multiply '((3 . 3) (0 . 2)) '())
  (degree '((5 . 3) (2 . 2) (0 . -1)))
)
