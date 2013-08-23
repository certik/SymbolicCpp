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


// lie1.cpp

#include <iostream>
#include "symbolicc++.h"
using namespace std;

Symbolic Z(const Symbolic &V, const Symbolic &x, const Symbolic &P)
{ return V * df(P, x); }

int main(void)
{
 int n = 4;
 Symbolic Q("Q"), V("V"), x("x"), ep("ep");
 
 Q = Q[x];
 V = V[x];

 Symbolic SD = ep * Z(V, x, Q);
 Symbolic RES = Q + ep * Z(V, x, Q);

 for(int j=1;j<=n;j++)
 {
  SD = ep * Z(V, x, SD) / (j + 1);
  RES = RES + SD;
 }

 cout << "RES = " << RES << endl << endl;

 Symbolic F = RES[V == x - (x^2)];

 cout << "F = " << F[Q == x] << endl;

 return 0;
}
