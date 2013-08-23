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


// metrics.cpp

#include <iostream>
#include "symbolicc++.h"
using namespace std;

int main(void)
{
 Symbolic du("du"), dv("dv");
 Symbolic u("u"), v("v");

 // du and dv are not commutative
 du = ~du; dv = ~dv;

 Symbolic x1 = cos(u)*sin(v);
 Symbolic x2 = sin(u)*sin(v);
 Symbolic x3 = cos(v);

 Symbolic dx1 = df(x1,u)*du + df(x1,v)*dv;
 Symbolic dx2 = df(x2,u)*du + df(x2,v)*dv;
 Symbolic dx3 = df(x3,u)*du + df(x3,v)*dv;

 Symbolic GT = dx1*dx1 + dx2*dx2 + dx3*dx3;

 GT = GT[ (cos(u)^2) == 1 - (sin(u)^2), (cos(v)^2) == 1 - (sin(v)^2) ];

 cout << GT << endl;

 return 0;
}
