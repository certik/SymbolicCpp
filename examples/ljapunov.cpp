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


// ljapunov.cpp
// (1) Iteration of logistic equation and variational equation
// (2) Variational equation obtained via exact differentiation
// (3) Determination of Ljapunov exponent

#include <iostream>
#include <math.h>
#include "verylong.h"
#include "rational.h"
#include "derive.h"
using namespace std;

int main(void)
{
   int N = 100;
   double x1, x = 1.0/3.0;
   double y = 1.0;
   Derive<double> C1(1.0);  // constant 1.0
   Derive<double> C4(4.0);  // constant 4.0
   Derive<double> X;

   cout << "i = 0   x = " << x << "   " << "y = " << y << endl;
   for(int i=1;i<=N;i++)
   {
      x1 = x;
      x = 4.0*x1*(1.0 - x1);

      X.set(x1);
      Derive<double> Y = C4*X*(C1 - X);

      y = df(Y)*y;
      cout << "i = " << i << "   "
           << "x = " << x << "   " << "y = " << y << endl;
   }
   double lam = log(fabs(y))/N;
   cout << "Approximation value for lambda = " << lam << endl;
   cout << endl;

   int M = 9;
   Rational<Verylong> u1;
   Rational<Verylong> u("1/3"), v("1");
   Rational<Verylong> K1("1");
   Rational<Verylong> K2("4");
   Derive<Rational<Verylong> > D1(K1); // constant 1
   Derive<Rational<Verylong> > D4(K2); // constant 4
   Derive<Rational<Verylong> > U;

   cout << "j = 0   u = " << u << "   " << "v = " << v << endl;
   for(int j=1;j<=M;j++)
   {
      u1 = u;
      u = K2*u1*(K1 - u1);
      U.set(Rational<Verylong>(u1));
      Derive<Rational<Verylong> > V = D4*U*(D1 - U);

      v = df(V)*v;
      cout << "j = " << j << "   "
           << "u = " << u << "   " << "v = " << v << endl;
   }
   lam = log(fabs(double(v)))/M;
   cout << "Approximation value for lambda = " << lam << endl;
   return 0;
}
