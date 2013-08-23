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


// vnorm.cpp

#include <iostream>
#include "vector.h"
#include "vecnorm.h"
using namespace std;

int main(void)
{
   Vector<int> v;
   v.resize(5,2);
   cout << "The size of vector v is " << v.size() << endl;
   cout << endl;

   Vector<double> a(4,-3.1), b;
   b.resize(4);
   b[0] = 2.3; b[1] = -3.6; b[2] = -1.2; b[3] = -5.5;

   // Different vector norms
   cout << "norm1() of a = " << norm1(a) << endl;
   cout << "norm2() of a = " << norm2(a) << endl;
   cout << "normI() of a = " << normI(a) << endl;
   cout << endl;

   cout << "norm1() of b = " << norm1(b) << endl;
   cout << "norm2() of b = " << norm2(b) << endl;
   cout << "normI() of b = " << normI(b) << endl;
   cout << endl;

   // The norm2() of normalized vectors a and b is 1
   cout << "norm2() of normalized a = " << norm2(normalize(a)) << endl;
   cout << "norm2() of normalized b = " << norm2(normalize(b)) << endl;

   return 0;
}
