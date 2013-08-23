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


// aresize.cpp

#include <iostream>
#include "array.h"
using namespace std;

int main(void)
{
   Array<double,1> M;
   M.resize(3);
   M[0] = 5.0;  M[1] = 8.0; M[2] = 4.0;
   cout << "M = \n" << M << endl; cout << endl;

   Array<double,2> A;
   A.resize(2,3);
   A[0][0] = 1.0;  A[0][1] = 3.0;  A[0][2] = 2.0;
   A[1][0] = 4.0;  A[1][1] = 5.0;  A[1][2] = 6.0;

   A.resize(dimensions<2>(3,4),2);
   cout << "A = \n" << A << endl;
   A.resize(2,2);
   cout << "A = \n" << A << endl;
   A.resize(dimensions<2>(4,4),9);
   cout << "A = \n" << A << endl;

   return 0;
}
