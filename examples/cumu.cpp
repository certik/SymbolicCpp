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


// cumu.cpp

#include <iostream>
#include "symbolicc++.h"
using namespace std;

Symbolic Taylor(Symbolic u,const Symbolic &x,int n)
{
   Symbolic series = u[x==0];
   int fac = 1;

   for(int j=1;j<=n;j++)
   {
      u = df(u,x); fac = fac * j;
      series += (u[x==0]*(x^j)/fac);
   }
   return series;
}

int main(void)
{
   int fac, n=5;
   Symbolic x("x"), a("a",5), b("b",5);
   Symbolic y, P, Q;

   fac = 1; P = a(0); Q = 0;
   for(int i=1;i<n;i++)
   {
      fac *= i;
      P += a(i)*(x^i)/fac;
      Q += b(i)*(x^i)/fac;
   }

   cout << "P = " << P << endl;
   cout << "Q = " << Q << endl; cout << endl;
   y = Taylor(exp(Q),x,5);

   cout << "Taylor series expansion of exp(Q) = " << endl;
   cout << y << endl;
   cout << endl;

   cout << "Coefficient of x :"<< endl;
   cout << "exp(Q) => " << y.coeff(x,1) << endl;
   cout << "  P    => " << P.coeff(x,1) << endl;
   cout << endl;

   cout << "Coefficient of x^2 :"<< endl;
   cout << "exp(Q) => " << y.coeff(x,2) << endl;
   cout << "  P    => " << P.coeff(x,2) << endl;
   cout << endl;

   cout << "Coefficient of x^3 :"<< endl;
   cout << "exp(Q) => " << y.coeff(x,3) << endl;
   cout << "  P    => " << P.coeff(x,3) << endl;
   cout << endl;

   cout << "Coefficient of x^4 :"<< endl;
   cout << "exp(Q) => " << y.coeff(x,4) << endl;
   cout << "  P    => " << P.coeff(x,4) << endl;

   return 0;
}
