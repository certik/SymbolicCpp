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


// grass.cpp

#include <iostream>
#include "symbolicc++.h"
using namespace std;

int main(void)
{
   int i,j,n=4;
   Symbolic e("e",n);
   Symbolic y,result;
   Symbolic A("A",n,n);

   // non-commutative e0, e1, e2, ... , en
   e = ~e;

   cout << e << endl;

   A(0,0) = 1; A(0,1) = 2; A(0,2) = 5; A(0,3) = 2;
   A(1,0) = 0; A(1,1) = 1; A(1,2) = 2; A(1,3) = 3;
   A(2,0) = 1; A(2,1) = 0; A(2,2) = 1; A(2,3) = 0;
   A(3,0) = 0; A(3,1) = 3; A(3,2) = 0; A(3,3) = 7;

   result = 1;
   for(i=0;i<n;i++) result *= A.row(i)*e;

   cout << result << endl; cout << endl;

   Equations rules;

   // set e(i)*e(i) to 0
   for(i=0;i<n;i++) rules = (rules,e(i)*e(i) == 0);

   // for all i>j, set e(i)*e(j) to -e(j)*e(i)
   for(i=0;i<n;i++)
      for(j=0;j<i;j++)
         rules = (rules,e(i)*e(j) == -e(j)*e(i));

   result = result.subst_all(rules);
   cout << "result = " << result << endl;
   return 0;
}
