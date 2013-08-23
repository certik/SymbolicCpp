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


// coeff2.cpp

#include <iostream>
#include "symbolicc++.h"
using namespace std;

int main(void)
{
   Symbolic a("a"), b("b"), c("c"), d("d");
   Symbolic y, z;

   y = -a*5*a*b*b*c+4*a-2*a*b*c*c+6-2*a*b+3*a*b*c-8*c*c*b*a+4*c*c*c*a*b-3*b*c;
   cout << "y = " << y << endl; cout << endl;

   z = a;         cout << y.coeff(z) << endl;
   z = b;         cout << y.coeff(z) << endl;
   z = c;         cout << y.coeff(z) << endl;
   z = a*a;       cout << y.coeff(z) << endl;
   z = a*b;       cout << y.coeff(z) << endl;
   z = d;         cout << y.coeff(z) << endl;
   cout << endl;

   // find coefficients of the constant term
   z = a;         cout << y.coeff(z,0) << endl;
   cout << y.coeff(1) << endl;
   return 0;
}
