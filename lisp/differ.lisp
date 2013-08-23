

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


; differ.lisp

(defun diff (ex v)                    ; d(ex)/dv
   (cond
      ((atom ex)
         (cond ((eq ex v) 1) (t 0)))  ; d(v)/dv = 1, d(constant)/dv = 0

      ((eq (car ex) '+)               ; d(a+b)/dv = da/dv + db/dv
         (list '+ (diff (cadr ex) v) (diff (caddr ex) v)))

      ((eq (car ex) '*)               ; d(a*b)/dv = da/dv * b + a * db/dv
         (list '+
            (list '* (diff (cadr ex) v) (caddr ex))
            (list '* (cadr ex) (diff (caddr ex) v))))

      ((eq (car ex) '/)               ; d(a/b)/dv = (da/dv)/b - (a/b*b)*db/dv
         (list '-
            (list '/ (diff (cadr ex) v) (caddr ex))
            (list '* `(/ ,(cadr ex) (* ,(caddr ex) ,(caddr ex)))
                      (diff (caddr ex) v))))

      ((eq (car ex) '-)               ; d(a-b)/dv = da/dv - db/dv
         (list '- (diff (cadr ex) v) (diff (caddr ex) v)))

      ((atom (car ex))                ; d/dv f(a) = f'(a) da/dv
         (let ( (form (assoc (car ex) '( (exp     (exp x))
                                         (sin     (cos x))
                                         (cos (- (sin x)))
                                         (ln      (/ 1 x)) ))) )
              (list '* (if form (subst (cadr ex) 'x (cadr form))
                               `(diff ,(car ex)  ,v))
                       (diff (cadr ex) v))))))
