

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


; countsub.lisp

(defun countsublists (lis)
 (cond
  ( (null lis) 0 )
  ( (atom lis) 0 )
  ( (atom (car lis)) (countsublists (cdr lis)) )
  ( t (+ 1
         (countsublists (car lis))
         (countsublists (cdr lis)) ) ) ) )

(countsublists '(a (1 2) (r s) (a b 23) z)) ; 3
(countsublists '())                         ; 0
