

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


; simp3.lisp

(load "simp2.lisp")

; remove definition of simp -- avoid warnings about redefinition
(unintern 'simp)

; redefine simp
(defun simp (f)
   (cond 
      ((atom f) f)                     ; f is an atom
      ((null (cdr f)) (simp (car f)))  ; f has only one element
      ((null (cddr f)) (simp-unary f)) ; f has two elements
      (t (simp-binary f)) ))           ; f has more than two elements

(defun simp-binary (f) 
   (let* ( (op   (car f))
           (opd1 (simp (cadr f)))           ; simplify first operand
           (opd2 (simp (caddr f)))          ; simplify second operand
           (zp1  (and (numberp opd1) (zerop opd1)))
           (zp2  (and (numberp opd2) (zerop opd2))) )
         (cond
           ((and (eq op '+)                    ; operation: x + y
            (cond (zp1 opd2)                   ; if x=0 return y
                  (zp2 opd1))))                ; if y=0 return x

           ((and (eq op '-)                    ; operation: x - y   
            (cond (zp1                         ; if x=0,
                   (cond (zp2 0)               ;    if y=0 return 0
                         (t (list '- opd2))))  ;    else return -y
                  (zp2 opd1)                   ; if y=0 return x
                  ((equal opd1 opd2) 0))))     ; if x=y return 0

           ((and (eq op '*)                    ; operation: x * y 
            (cond ((or zp1 zp2) 0)             ; if x=0 or y=0 return 0
                  ((equal 1 opd1) opd2)        ; if x=1 return y 
                  ((equal 1 opd2) opd1))))     ; if y=1 return x

           ((and (eq op '/)                    ; operation: x / y
            (cond (zp1 0)                      ; if x=0 return 0
                  (zp2 'infinity)              ; if y=0 return infinity
                  ((equal 1 opd2) opd1))))     ; if y=1 return x

           ((and (eq op 'power)                ; operation: x^y
            (cond (zp1 0)                      ; if x=0 return 0
                  ((or zp2 (onep opd1)) 1)     ; if y=0 or x=1 return 1
                  ((equal 1 opd2) opd1))))     ; if y=1 return x

           (t (list op opd1 opd2)))))
