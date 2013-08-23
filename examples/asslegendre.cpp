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


// asslegendre.cpp
// Associate Legendre Polynomial
// P_l^|m|(w) = (1-w^2)^(|m|/2) (d/dw)^|m| P_l(w)
// Recursion formula (n+1)P_{n+1}(x) = (2n+1)xP_n(x)-nP_{n-1}(x)

#include <iostream>
#include "symbolicc++.h"
#include "asslegendre.h"
using namespace std;

int main(void)
{
   int n=5;
   Symbolic x("x") ,Y;
   AssLegendre P(x);

   for(int i=0;i<n;i++)
   {
      for(int j=0;j<=i;j++)
      {
         P.redefine(i,j);
         Y = P.current();
         cout << "P(" << i << "," << j << ") = " << Y << endl;
      }
      cout << endl;
   }
   return 0;
}
