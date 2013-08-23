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


// ruelle.cpp

#include <iostream>
#include <cmath>
#include "symbolicc++.h"
using namespace std;

template <class T> T f(T x,T y,T r)
{ return r*(T(3)*y+T(1))*x*(T(1)-x); }

template <class T> T g(T x,T y,T r)
{ return r*(T(3)*x+T(1))*y*(T(1)-y); }

int main(void)
{
  int T = 1000;   // number of iterations
  double x2, y2, u2, v2;
  Symbolic x("x"), x1("x1"), y("y"), y1("y1"), r("r"),
           u("u"), u1("u1"), v("v"), v1("v1");
  x1 = f(x,y,r); y1 = g(x,y,r);
  cout << "x1 = " << x1 << endl;
  cout << "y1 = " << y1 << endl;
  u1 = df(x1,x)*u + df(x1,y)*v; // variational equation
  v1 = df(y1,y)*v + df(y1,x)*u; // variational equation
  cout << "u1 = " << u1 << endl;
  cout << "v1 = " << v1 << endl; cout << endl;

  // initial values
  Equations values = (x == 0.3, y == 0.4, r == 1.0834, u == 0.5, v == 0.6);
  for(int t=1;t<T;t++)
  {
   x2 = x1[values]; y2 = y1[values];
   u2 = u1[values]; v2 = v1[values];
   values = (r == 1.0834, x == x2, y == y2, u == u2, v == v2);
   cout << "The Liapunov exponent for t = " << t << " is "
        << log(fabs(double(rhs(values,u)))
              +fabs(double(rhs(values,v))))/t << endl;
  }
  return 0;
}
