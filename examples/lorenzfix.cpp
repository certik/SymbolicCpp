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


// lorenzfix.cpp

#include <iostream>
#include <cmath>
#include "symbolicc++.h"
using namespace std;

int main(void)
{   
  int i, j;
  Symbolic u("u",3), V("V",3), A("A",3,3), s("s"), b("b"), r("r");

  V(0) = s*(u(1) - u(0));         // Lorenz model
  V(1) = -u(0)*u(2) + r*u(0) - u(1);  // Lorenz model
  V(2) = u(0)*u(1) - b*u(2);        // Lorenz model

  for(i=0;i<3;i++)
   for(j=0;j<3;j++)
    A(i,j) = df(V(i),u(j));

  Symbolic lambda("lambda");
  Symbolic chareq;

  // characteristic equation
  chareq = det(lambda*A.identity() - A);

  // coefficients
  Symbolic c0 = chareq.coeff(lambda,0);
  Symbolic c1 = chareq.coeff(lambda,1);
  Symbolic c2 = chareq.coeff(lambda,2);

  Symbolic Q = (3*c1 - c2*c2)/9;
  Symbolic R = (9*c2*c1 - 27*c0 - 2*c2*c2*c2)/54;
  Symbolic D = Q*Q*Q + R*R;

  // parameter values
  Equations values = (s == 16.0, b == 4.0, r == 40.0);

  // fixed point (0,0,0)
  values = (values, u(0) == 0.0, u(1) == 0.0, u(2) == 0.0);

  double q = Q[values];
  double rR = R[values]; 
  double d = D[values];
  double nc2 = c2[values];

  if(rR != 0 && q < 0 && d <= 0.0)
  {
   double theta = acos(rR/sqrt(-q*q*q));
   double PI = 3.14159;
   double lamb1 = 2.0*sqrt(-q)*cos(theta/3.0) - nc2/3.0;
   double lamb2 = 2.0*sqrt(-q)*cos((theta+2.0*PI)/3.0)-nc2/3.0;
   double lamb3 = 2.0*sqrt(-q)*cos((theta+4.0+PI)/3.0)-nc2/3.0;

   cout << "lamb1 = " << lamb1 << endl; 
   cout << "lamb2 = " << lamb2 << endl;
   cout << "lamb3 = " << lamb3 << endl;
  }
  return 0;
}
