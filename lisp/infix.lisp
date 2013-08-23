

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


; infix.lisp

; define the operators in decreasing precedence levels
(defvar precedence '( (* /) (+ -) ))

; convert any infix expressions with binary operations from ops
; to prefix form, first? specifies if this is the first element
; in a list
(defun convert (x ops first?)
  (cond
  ; pull out the first element of a single element list
  ( (and (listp x) (= (length x) 1) first?)
    (convert (car x) ops t) )
  ; a list of at least 3 elements: a op b ... -> (op a b) ...
  ;  op in ops has greater or equal precedence to other infix operators
  ;  in the expression to be converted
  ( (and (listp x) (> (length x) 2) (member (cadr x) ops))
    (convert (cons (list (cadr x) (car x) (caddr x))
                   (cdddr x) ) ops nil) )
  ; a pair of two elements - try to convert each element
  ( (consp x)  (cons (convert (car x) ops t)
    (convert (cdr x) ops nil)) )
  ; not a list to be converted
  ( t x ) ) )

; apply the conversion procedure for operators
; of decreasing precedence
(defun apply-precedence (pl x)
 (if (null pl) (convert x pl t)
     ; else apply the next precedence level
     ; after conversion for the current
     (apply-precedence (cdr pl) (convert x (car pl) t)) ) )

; convert an infix expression to prefix
; x - a list representing the expression to convert
(defun infix->prefix (&rest x) (apply-precedence precedence x) )

; macro for use within LISP
(defmacro infix (&rest x) (infix->prefix x))

; examples of infix within LISP
(princ (infix   2 + 3 * 5)) (newline)
(princ (infix (2 + 3) * 5)) (newline)

; example showing that infix is context sensitive
(labels ( (+ (x y) (append x y))
          (* (x y) (mapcar #'(lambda (x) (cons x y)) x)) )
      (princ (infix '(a b c) + '(d e f) * '(g h i))) (newline)
      (princ (infix ('(a b c) + '(d e f)) * '(g h i))) (newline) )
