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


// newton.cpp

#include <iostream>
#include <iomanip>   // for setprecision()
#include "symbolicc++.h"
using namespace std;

template <class T> T f(T x)  // f(x)
{ return 4*x-cos(x); }

int main(void)
{
  int N=7;
  double y, u0 = -1.0;
  Symbolic x("x"), ff, ff1, ff2, C;
  Equation value = (x==u0);
  ff = f(x); ff1 = df(ff,x); ff2 = df(ff1,x);

  // Set numerical precision to 10 decimal places
  cout << setprecision(10);
  cout << "f(x) = " << ff << endl;
  cout << "f'(x) = " << ff1 << endl;
  cout << "f''(x) = " << ff2 << endl;
  cout << endl;

  // ======= Condition for convergence =======
  C = ff*ff2/ff1;
  cout << "C = " << C << endl;
  cout << "|C(x=u0)| = " << fabs(double(C[x==u0])) << endl;
  cout << endl;

  // ======= Symbolic computation =========
  for(int i=0;i<N;i++)
  {
   y = double((x-ff/ff1)[value]);
   value = (x==y);
   cout << "x = " << x[value] << endl;
  }
  return 0;
}
