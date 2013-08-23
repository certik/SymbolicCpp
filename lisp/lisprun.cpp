/*
    SymbolicC++ : An object oriented computer algebra system written in C++

    Copyright (C) 2008 Yorick Hardy and Willi-Hans Steeb

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along
    with this program; if not, write to the Free Software Foundation, Inc.,
    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
*/


// lisprun.cpp

#include <iostream>
#include "lisp.h"
#include "verylong.h"
using namespace std;

typedef Type<int> l_int;
typedef Type<char> l_char;
typedef Type<double> l_double;
typedef Type<Verylong> l_verylong;

Element *l_plus(Element *e)
{
 if(is_nonempty_lisp_list(e))
 {
  if(is_nonempty_lisp_list(cdr(e)))
  {
   //assume int for calculations
   Element *param1,*param2;
   param1=car(e); param2=car(cdr(e));
   if(param2!=&nil)
    return l_int(((l_int*)param1)->value()
                +((l_int*)param2)->value()).clone();
  }
 }
 cerr<<"Plus takes a list of two integers as arguments"<<endl;
 return &nil;
}

Element* length_list(Element *e)
{
 l_int zero(0),one(1);
 
 //stop C++ recursion
 if(e==&nil) return cons(&zero,&nil);
                                  // Equivalent LISP code
 return cond(cons (               // (cond 
  cons(null(e),cons(&zero,&nil)), // ((null e) 0)
  cons(                           // (t (l_plus 1 (length_list (cdr e)))))
  cons(&t,cons(l_plus(cons(&one,length_list(cdr(e)))),&nil)),&nil)));
}

int main(void)
{
   // Define two "atoms"
   l_int a(1);    // 1
   l_char b('a'); // a

   cout << "Examples on the cons function:" << endl;

   Pair *A = cons(&a,&nil);  // in Lisp :  ( 1 )
   Pair *B = cons(&b,&nil);  // in Lisp :  ( a )

   Pair *C = cons(&a,&b);    // in Lisp : (cons '1 'a)
   cout << "(cons '1 'a)     => " << C << endl;

   C = cons(&a,B);           // in Lisp (cons '1 '(a))
   cout << "(cons '1 '(a))   => " << C << endl;

   C = cons(A,&b);           // in Lisp (cons '(1) 'a)
   cout << "(cons '(1) 'a)   => " << C << endl;

   C = cons(A,B);            // in Lisp (cons '(1) '(a))
   cout << "(cons '(1) '(a)) => " << C << endl;
   cout << endl;

   cout << "Examples on the append function:" << endl;

   // (setq D1 '(1 2 3))
   Pair *D1 = cons(new l_int(1),cons(new l_int(2),cons(new l_int(3),&nil)));

   // (setq D2 '(a b c))
   Pair *D2 = cons(new l_char('a'),cons(new l_char('b'),
		   cons(new l_char('c'),&nil)));

   Pair *D = (Pair*)append(D1,D2);
   cout << "(append '(1 2 3) '(a b c)) => " << D << endl;
   cout << endl;

   cout << "Examples on the car and cdr functions:" << endl;

   // setq E '((1 2 3) (a b c))
   Pair *E = cons(D1,cons(D2,&nil));

   cout << "(car '((1 2 3) (a b c)))       => " << car(E)      << endl;
   cout << "(car (car '((1 2 3) (a b c)))) => " << car(car(E)) << endl;
   cout << "(cdr '((1 2 3) (a b c)))       => " << cdr(E)      << endl;
   cout << "(car (cdr '((1 2 3) (a b c)))) => " << car(cdr(E)) << endl;
   cout << "(cdr (cdr '((1 2 3) (a b c)))) => " << cdr(cdr(E)) << endl;
   cout << "(car 'a) => " << car(&a) << endl;
   cout << endl;

   // Abstract data types
   cout << "Applications with the abstract data type: Verylong" << endl;

   l_verylong v(Verylong("123456789012"));
   l_double r(3.14159);
   Pair *Very = cons(&v,cons(&r,&nil));
   cout << "(cons 'v '(r)) => " << Very << endl;

   cout << endl << "Applications on the cond function:" << endl;

   //((nil r) (t v))
   cout << "(cond (nil 3.14159) (t 123456789012)) => "
        << cond(cons(cons(&nil,cons(&r,&nil)),
                     cons(cons(&t,cons(&v,&nil)),&nil)))
        <<endl;
   //((t r) (nil v))
   cout << "(cond (t 3.14159) (nil 123456789012)) => "
        << cond(cons(cons(&t,cons(&r,&nil)),
                     cons(cons(&nil,cons(&v,&nil)),&nil)))
        <<endl;

   Pair *test=cons(&a,cons(&b,cons(&v,cons(&r,&nil))));
   cout << endl << "(a b v r) => " << test << endl;
   cout << "(length_list (a b v r)) => " << length_list(test) << endl;

   return 0;
}
