

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


; symbol.lisp

(load "infix.lisp")

(defun sum     (l) (cons '+ l))
(defun product (l) (cons '* l))

(defun symbol?  (s) (symbolp s))
(defun number?  (n) (numberp n))
(defun sum?     (s) (and (consp s) (equal (car s) '+)))
(defun product? (s) (and (consp s) (equal (car s) '*)))

(defun add (a b) `(+ ,a ,b))
(defun mul (a b) `(* ,a ,b))

(defun ifrec (again f)
 (if again #'(lambda (x) (funcall f nil x)) #'(lambda (x) x)) )

(defun split (p l)
 (if (null l) (list nil nil nil)
     (if (funcall p (car l)) (list nil (car l) (cdr l))
         (let ( (s (split p (cdr l))) )
              (cons (cons (car l) (car s)) (cdr s)) ) ) ) )

(defun expand1 (again s)
 (cond ( (symbol? s) s )
       ( (number? s) s )
       ; sum or product with one element in the list
       ( (null (cddr s)) (expand (cadr s)) )
       ; expand each element in the sum, and absorb sums
       ( (sum? s)
         (funcall (ifrec again #'expand1) (expand2 s #'sum? #'sum)) )
       ; apply distributivity for product, when it contains a sum
       ; otherwise expand and absorb
       ( t (let* ( (s1 (split #'sum? (cdr s))            )
                   (a  (car s1)                          )
                   ; cadr -> sum, cdadr -> list from sum
                   (s2 (and (sum? (cadr s1)) (cdadr s1)) )
                   (b  (caddr s1)                        ) )
                (if (null s2)
                    ; no sum found
                    (funcall (ifrec again #'expand1)
                             (expand2 s #'product? #'product) )
                    ; sum found
                    (expand1 again
                      (sum (mapcar (if (and (null a) (null b))
                                       #'(lambda (x) x)
                                       #'(lambda (x)
                                          (product (append a (list x) b))))
                                   s2 ) ) ) ) ) ) ) )

; implement expandabsorb for both sum and product
; using p? <- sum?, c <- sum or p? <- product?, c <- product
(defun expand2 (s p? c)
 (labels ( (absorb (x) (if (funcall p? x) (cdr x) (list x)))
           (expandabsorb (x) (absorb (expand x))) )
         (let ( (newlist (reduce #'append
                                   (mapcar #'expandabsorb
                                           (cdr s) ) ) ) )
              (funcall c newlist) ) ) )

(defun expand (s) (expand1 t s))

(defun tostring (s)
 (cond ( (symbol? s) (format nil "~A" s) )
       ( (number? s) (format nil "~A" s) )
       ( (null (cdr s)) "")
       ; sum or product of one symbol
       ( (and (symbol? (cadr s)) (null (cddr s)))
         (format nil "~A" (cadr s)) )
       ; sum or product of one number
       ( (and (number? (cadr s)) (null (cddr s)))
         (format nil "~A" (cadr s)) )
       ; sum or product of one element
       ( (and (null (cddr s)))
         (format nil "(~A)" (tostring (cadr s))) )
       ( (sum? s)
         (let ( (summand1 (tostring (sum (cons (cadr s) nil))))
                (summand2 (tostring (sum (cddr s)))) )
              (format nil "~A + ~A" summand1 summand2) ) )
       ( (product? s)
         (let ( (factor1 (tostring (cadr s)))
                (factor2 (tostring (product (cddr s)))) )
              (format nil "~A~A" factor1 factor2) ) ) ) )

; examples
(labels ( (+ (a b) (add a b))
          (* (a b) (mul a b))
          (print (a) (format t "~A~%" a)) )
        (defvar a 'a)
        (defvar b 'b)
        (defvar c (infix 3 * (a + b) * a))
        (defvar d 'd)
        (defvar e (infix 3.0 * d * (d + d)))
        (print (tostring c))
        (print (tostring (expand c)))
        (print (tostring (expand (infix a + b + a))))
        (print (tostring e)) )
