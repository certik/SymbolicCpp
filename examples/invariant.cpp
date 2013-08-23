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

// invariant.cpp

#include <iostream>
#include "symbolicc++.h"
using namespace std;

int main(void)
{
 Symbolic x("x"), x1("x1"), x2("x2"), d("d");
 Symbolic f = 2*(x^2) - 1;
 Symbolic g = x2 - 2*(x1^2) + 2*(x2^2) + d*(1 + x2 - 2*(x1^2));

 cout << f[ x == f ] << endl;
 cout << g[ x1 == x, x2 == f ] << endl;

 if( f[ x == f ] == g[ x1 == x, x2 == f ] )
  cout << "Invariant." << endl;

 return 0;
}
