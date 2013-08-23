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


// legendre.cpp
// Recursion formula (n+1)P_{n+1}(x) = (2n+1)xP_n(x)-nP_{n-1}(x)

#include <iostream>
#include "symbolicc++.h"
#include "legendre.h"
using namespace std;

int main(void)
{
   int n=4;
   Symbolic x("x");
   Legendre P(n,x);

   // Calculate the first few Legendre polynomials
   cout << "P(0) = " << P << endl;
   for(int i=1;i<=n;i++)
   {
      P.step();
      cout << "P("<< i << ") = " << P << endl;
   }
   cout << endl;

   // Another way to access the Legendre polynomial
   cout << "P(1) = " << P(1) << endl;
   cout << "P(2) = " << P(2) << endl;
   cout << "P(3) = " << P(3) << endl;
   cout << "P(4) = " << P(4) << endl; cout << endl;

   // Show that the Legendre differential equation is satisfied for n = 4
   Symbolic result;
   result = df((1-x*x)*df(P.current(),x),x) + (n*(n+1))*P.current();
   cout << result << endl;  // ==> 0
   return 0;
}
