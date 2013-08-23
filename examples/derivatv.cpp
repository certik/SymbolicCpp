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


// derivatv.cpp

#include <iostream>
#include "symbolicc++.h"
using namespace std;

int main(void)
{
   int i;
   Symbolic x("x"), y, z("z");

   y = 1/(1-x) + 2*(x^3) - z;
   cout << "y = " << y << endl;

   for(i=0;i<8;i++)
   { y = df(y,x); cout << "y = " << y << endl; }
   cout << endl;

   Symbolic u, v("v");

   u = (v^(Symbolic(3)/5))/3 - 2*(v^(Symbolic(1)/5))/7 + Symbolic(1)/6;
   cout << "u = " << u << endl;

   for(i=0;i<8;i++)
   { u = df(u,v); cout << "u = " << u << endl; }
   return 0;
}
