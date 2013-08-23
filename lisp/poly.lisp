

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


; poly.lisp

(defun add (P Q)
   (cond
      ((null P) Q)
      ((null Q) P)
      ((> (caar P) (caar Q)) (cons (car P) (add (cdr P) Q)))
      ((> (caar Q) (caar P)) (cons (car Q) (add P (cdr Q))))
      ((zerop (+ (cdar P) (cdar Q))) (add (cdr P) (cdr Q)))
      (t (cons (cons (caar P) (+ (cdar P) (cdar Q)))
         (add (cdr P) (cdr Q))))))

(defun negate (P)
   (mapcar #'(lambda (x) (cons (car x) (- (cdr x)))) P))

(defun subtract (P Q)
   (add P (negate Q)))

(defun multiply (P Q)
   (cond
      ((or (null P) (null Q)) nil)
      (t (cons (cons (+ (caar P) (caar Q)) (* (cdar P) (cdar Q)))
               (add (multiply (list (car P)) (cdr Q))
                    (multiply (cdr P) Q))))))

(defun degree (P)
   (if (null P) 0 (caar P)))
