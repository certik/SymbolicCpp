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

// Compile with:
// g++ -O3 -march=native -ffast-math -funroll-loops -I../headers expand.cpp

// expand.cpp

#include <iostream>
#include "symbolicc++.h"
using namespace std;

int main(void)
{
 Symbolic x("x"), y("y"), z("z"), w("w"), e, f;

 // This 3 should be 15:
 cout << "Calculating e = (x+y+z+w)^3" << endl;
 e = ((x+y+z+w)^3);
 cout << "Calculating f = e*(e+1)" << endl;
 f = e*(e+1);
 cout << "Done. Printing:" << endl;
 cout << " e = " << e << endl;
 cout << " f = " << f << endl;
 cout << endl;
 return 0;
}
