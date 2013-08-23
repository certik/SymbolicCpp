

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


; simp1.lisp

(defun simp-unary (f)
   (let* ( (op  (car f))
           (opd (cadr f))
           (z?  (and (numberp opd) (zerop opd)))   ; zero
           (o?  (equal 1 opd))                     ; one
           (p?  (equal 'pi opd)) )                 ; pi
         (cond
           ((eq op '+)               opd)   ; + x       => x
           ((and (eq op '-)      z?)   0)   ; - 0       => 0
           ((and (eq op 'exp)    z?)   1)   ; exp(0)    => 1
           ((and (eq op 'log)    o?)   0)   ; log(1)    => 0
           ((and (eq op 'sin)    z?)   0)   ; sin(0)    => 0
           ((and (eq op 'cos)    z?)   1)   ; cos(0)    => 1
           ((and (eq op 'sin)    p?)   0)   ; sin(pi)   => 0
           ((and (eq op 'cos)    p?)  -1)   ; cos(pi)   =>-1
           ((and (eq op 'arcsin) z?)   0)   ; arcsin(0) => 0
           ((and (eq op 'arctan) z?)   0)   ; arctan(0) => 0
           ((and (eq op 'sinh)   z?)   0)   ; sinh(0)   => 0
           ((and (eq op 'cosh)   z?)   1)   ; cosh(0)   => 1
           (t (list op opd)))))
