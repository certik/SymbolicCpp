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


// lie2.cpp

#include <iostream>
#include "symbolicc++.h"
using namespace std;

Symbolic Z(const Symbolic &V1, const Symbolic &V2,
           const Symbolic &x1, const Symbolic &x2, const Symbolic &P)
{ return V1 * df(P, x1) + V2 * df(P, x2); }

int main(void)
{
 int n = 4;
 Symbolic Q1("Q1"), Q2("Q2"), V1("V1"), V2("V2"), x1("x1"), x2("x2"), ep("ep");
 
 Q1 = Q1[x1,x2];
 Q2 = Q2[x1,x2];
 V1 = V1[x1,x2];
 V2 = V2[x1,x2];

 Symbolic SD1= ep * Z(V1, V2, x1, x2,  Q1);
 Symbolic SD2= ep * Z(V1, V2, x1, x2,  Q2);
 Symbolic RES1 = Q1 + ep * Z(V1, V2, x1, x2, Q2);
 Symbolic RES2 = Q1 + ep * Z(V1, V2, x1, x2, Q2);

 for(int j=1;j<=n;j++)
 {
  SD1 = ep * Z(V1, V2, x1, x2, SD1) / (j + 1);
  SD2 = ep * Z(V1, V2, x1, x2, SD2) / (j + 1);
  RES1 = RES1 + SD1;
  RES2 = RES2 + SD2;
 }

 cout << "RES1 = " << RES1 << endl << endl;
 cout << "RES2 = " << RES2 << endl << endl;

 Symbolic F1 = RES1[V1 == x1 - x1*x2, V2 == -x2 + x1*x2];
 Symbolic F2 = RES2[V1 == x1 - x1*x2, V2 == -x2 + x1*x2];

 cout << "F1 = " << F1[Q1 == x1, Q2 == x2] << endl << endl;
 cout << "F2 = " << F2[Q1 == x1, Q2 == x2] << endl;

 return 0;
}
