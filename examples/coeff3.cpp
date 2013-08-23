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


// coeff3.cpp

#include <iostream>
#include "symbolicc++.h"
using namespace std;

int main(void)
{
   int i;
   Symbolic x("x"), p("p");
   Symbolic w = 0;

   for(i=-5;i<=5;i++) w += (6+i)*(p^(5-i))*(x^i)/(6-i);

   cout << "w = " << w << endl; cout << endl;

   cout << "The coefficients are" << endl;
   for(i=-5;i<=5;i++) cout << w.coeff(x,i) << endl;
   return 0;
}
