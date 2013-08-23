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


// Euler-Lagrange equation
// lag.cpp

#include <iostream>
#include "symbolicc++.h"
using namespace std;

int main(void)
{
 Symbolic om("om"), ts("ts"), u1("u1"), u2("u2"), u1d("u1d"), u2d("u2d"),
          u1dd("u1dd"), u2dd("u2dd");

 Symbolic L = ((u1d^2) + (u2d^2))/2
            + om*(u1*u2d - u1d*u2)
            + om*(u1*u1d + u2*u2d)*tan(om*ts)
            + (om^2)*((u1^2) + (u2^2))*(sec(om*ts)^2)/2;

 Symbolic LUD1 = df(L, u1d);
 Symbolic res1 = df(LUD1, ts)
                   + u1d*df(LUD1, u1)
                   + u1dd*df(LUD1, u1d) - df(L, u1);

 Symbolic LUD2 = df(L, u2d);
 Symbolic res2 = df(LUD2, ts)
                   + u2d*df(LUD2, u2)
                   + u2dd*df(LUD2, u2d) - df(L, u2);

 res1 = res1[(sec(om*ts)^2) == 1 + (tan(om*ts)^2)];
 res2 = res2[(sec(om*ts)^2) == 1 + (tan(om*ts)^2)];
 res1 = res1[(sin(om*ts)^2) == 1 - (cos(om*ts)^2)];
 res2 = res2[(sin(om*ts)^2) == 1 - (cos(om*ts)^2)];

 cout << "res1 = " << res1 << endl;
 cout << "res2 = " << res2 << endl;

 Symbolic v1("v1"), v2("v2");

 Symbolic eq1 = res1[ u1dd == df(v1[ts], ts, 2), u2d == df(v2[ts], ts) ];
 Symbolic eq2 = res2[ u2dd == df(v2[ts], ts, 2), u1d == df(v1[ts], ts) ];
 
 cout << "eq1 = " << eq1 << endl;
 cout << "eq2 = " << eq2 << endl;
 
 return 0;
}

