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


// conser.cpp

#include <iostream>
#include "symbolicc++.h"
using namespace std;

int main(void)
{
 Symbolic u("u"), x1("x1"), x2("x2");
 
 u = u[x1, x2];

 Symbolic S = df(u, x2);
 Symbolic X = df(u, x1);

 Symbolic A = S[ u == (df(u,x1)^2)/2, df(df(u,x1),x2) == sin(u) ];
 Symbolic B = X[ u == cos(u), df(df(u,x1),x2) == sin(u)];

 cout << "A = " << A << endl;
 cout << "B = " << B << endl;

 Symbolic CL = A + B;

 cout << "CL = " << CL << endl;

 return 0;
}
