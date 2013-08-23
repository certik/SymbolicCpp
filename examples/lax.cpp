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


// lax.cpp

#include <iostream>
#include "symbolicc++.h"
using namespace std;

int main(void)
{
  Symbolic L("L",3,3), A("A",3,3), Lt("Lt",3,3); // Lt=dL/dt
  Symbolic a1("a1"), a2("a2"), b1("b1"), b2("b2"), b3("b3"),
           a1t, a2t, b1t, b2t, b3t;

  L(0,0) = b1; L(0,1) = a1;  L(0,2) = 0;
  L(1,0) = a1; L(1,1) = b2;  L(1,2) = a2;
  L(2,0) = 0;  L(2,1) = a2;  L(2,2) = b3;
  A(0,0) = 0;  A(0,1) = -a1; A(0,2) = 0;
  A(1,0) = a1; A(1,1) = 0;   A(1,2) = -a2;
  A(2,0) = 0;  A(2,1) = a2;  A(2,2) = 0;

  Lt = A*L - L*A; 
  cout << "Lt =\n" << Lt << endl;

  b1t = Lt(0,0); b2t = Lt(1,1); b3t = Lt(2,2);
  a1t = Lt(0,1); a2t = Lt(1,2);
  cout << "b1t = " << b1t << ", b2t = " << b2t 
       << ", b3t = " << b3t << endl;
  cout << "a1t = " << a1t << ", a2t = " << a2t << endl;
  cout << endl;
   
  // Show that I(0),I(1),I(2) are first integrals
  int n = 3;
  Symbolic result;
  Symbolic I("I",n);

  I(0) = L.trace();       cout << "I(0) = " << I(0) << endl;
  I(1) = (L*L).trace();   cout << "I(1) = " << I(1) << endl;
  I(2) = L.determinant(); cout << "I(2) = " << I(2) << endl;
  cout << endl;

  for(int i=0;i<n;i++)
  {
   result = b1t*df(I(i),b1) + b2t*df(I(i),b2) + b3t*df(I(i),b3)
            + a1t*df(I(i),a1) + a2t*df(I(i),a2);

   cout << "result" << i+1 << " = " << result << endl;
  }
  return 0;
}

