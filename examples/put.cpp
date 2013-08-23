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


// put.cpp

#include <iostream>
#include "symbolicc++.h"
using namespace std;

int main(void)
{
   Symbolic a("a"), b("b"), c("c"), w, y;

   // Test (1)
   y = (a+b)*(a+sin(cos(a)^2))*b;
   cout << "y = " << y << endl;

   y = y[b==c+c]; cout << "y = " << y << endl;

   y = y[cos(a)*cos(a)==1-sin(a)*sin(a)];
   cout << "y = " << y << endl; cout << endl;

   // Test (2)
   y = sin(cos(a)^2) + b; cout << "y = " << y << endl;

   y = y[cos(a)*cos(a)==1-sin(a)*sin(a)];
   cout << "y = " << y << endl; cout << endl;

   // Test (3)
   a = a[c]; b = b[c];
   y = 2*a*df(b*a*a,c)+a*b*c; cout << "y = " << y << endl;

   y = y[a==cos(c)]; cout << "y = " << y << endl;

   w = df(b,c); y = y[w==exp(a)]; cout << "y = " << y << endl;

   return 0;
}
