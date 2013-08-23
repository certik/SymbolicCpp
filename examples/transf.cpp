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


// transf.cpp

#include <iostream>
#include "symbolicc++.h"
using namespace std;

int main(void)
{
 // group SO(1,1)
 Symbolic A("A",2,2);
 Symbolic x("x"), x1("x1"), x2("x2"),
          alpha("alpha"), beta("beta"), gamma("gamma"), nu("nu");
 Symbolic result;

 A(0,0) = cosh(x); A(0,1) = sinh(x);
 A(1,0) = sinh(x); A(1,1) = cosh(x);

 Symbolic v("v",2);
 v(0) = x1; v(1) = x2;

 Symbolic w = A * v;
 
 result = (w(0)^2) + (w(1)^2);
 result = result.subst(cosh(x)*cosh(x), 1 + sinh(x)*sinh(x));
 cout << result << endl;

 // group SO(2)
 Symbolic B("B",2,2);
 Symbolic i = sqrt(Number<int>(-1));

 B(0,0) = cos(x); B(0,1) = -sin(x);
 B(1,0) = sin(x); B(1,1) =  cos(x);

 Symbolic s = B * v;
 
 result = (s(0)^2) + (s(1)^2);
 result = result.subst(cos(x)*cos(x), 1 - sin(x)*sin(x));
 cout << result << endl;

 // group U(2), calculating the determinant
 Symbolic U("U",2,2);
 U(0,0) = exp(i*alpha)*cos(nu);
 U(0,1) = exp(i*gamma)*sin(nu);
 U(1,0) = -exp(i*(beta-gamma))*sin(nu);
 U(1,1) = exp(i*(beta-alpha))*cos(nu);
 
 result = det(U);
 result = result.subst(cos(nu)*cos(nu), 1 - sin(nu)*sin(nu));
 cout << result << endl;

 return 0;
}
