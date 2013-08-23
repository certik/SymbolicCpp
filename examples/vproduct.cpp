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


// vproduct.cpp

#include <iostream>
#include "vector.h"
using namespace std;

int main(void)
{
   Vector<double> A(3), B(3), C(3), D(3);
   A[0] = 1.2; A[1] = 1.3; A[2] = 3.4;
   B[0] = 4.3; B[1] = 4.3; B[2] = 5.5;
   C[0] = 6.5; C[1] = 2.6; C[2] = 9.3;
   D[0] = 1.1; D[1] = 7.6; D[2] = 1.8;
   cout << A%(B%C) + B%(C%A) + C%(A%B) << endl;
   cout << (A%B)%(C%D) << endl;
   cout << B*(A|C%D)-A*(B|C%D) << endl;
   cout << C*(A|B%D)-D*(A|B%C) << endl;
   // precedence of | is lower than <<
   cout << (A|B%C) - (A%B|C) << endl;
   return 0;
}
