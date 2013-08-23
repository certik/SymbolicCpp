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


// taylor1.cpp

#include <iostream>
#include "symbolicc++.h"
using namespace std;

int factorial(int N)
{
   int result=1;
   for(int i=2;i<=N;i++) result *= i;
   return result;
}

int main(void)
{
   int i, j, n=4;
   Symbolic u("u"), x("x"), result;
   Symbolic u0("",n), y("",n);
   u = u[x];
   u0(0) = u*u+x;
   for(j=1;j<n;j++) u0(j) = df(u0(j-1),x);

   cout << u0 << endl;

   // initial condition u(0)=1
   u0(0) = u0(0)[u==1,x==0];

   y(0) = u;
   for(i=1;i<n;i++) y(i) = df(y(i-1),x);

   // substitution of initial conditions
   for(i=1;i<n;i++)
      for(j=i;j>0;j--) u0(i) = u0(i)[y(j)==u0(j-1)];
   cout << u0 << endl;

   for(i=0;i<n;i++) u0(i) = u0(i)[u == 1];
   cout << u0 << endl;

   // Taylor series expansion
   result = 1;
   for(i=0;i<n;i++)
      result += (Symbolic(1)/factorial(i+1))*u0(i)*(x^(i+1));
   cout << "u(x) = " << result << endl;
   return 0;
}
