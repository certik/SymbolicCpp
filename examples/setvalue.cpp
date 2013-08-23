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


// setvalue.cpp

#include <iostream>
#include "symbolicc++.h"
using namespace std;

int main(void)
{
   Symbolic x("x"), y("y"), z("z");
   double   c1 = 0.5, c2 = 1.2;

   y = x*x+z/2.0;
   cout << "y = " << y << endl;

   cout << "Put x = " << c1 << ", z = " << c2 << endl;
   cout << "The value of y = " << y
        << " is " << double(y[x==c1,z==c2]) << endl;
   cout << endl;

   y = x*x*z + 0.7*z - x*z;
   cout << "y = " << y << endl;

   cout << "Put x = " << c1 << endl;
   cout << "The value of y is " << y[x==c1] << endl;
   cout << endl;

   cout << "Put z = " << c2 << endl;
   cout << "The value of y is "
        << double(y[x==c1, z==c2]) << endl;

   return 0;
}
