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


// trace.cpp

#include <iostream>
#include "matrix.h"
using namespace std;

int main(void)
{
   Matrix<int> A(3,3), B(3,3,-1);

   A[0][0] = 2; A[0][1] = -1; A[0][2] =  1;
   A[1][0] = 1; A[1][1] = -2; A[1][2] = -1;
   A[2][0] = 3; A[2][1] =  2; A[2][2] = 2;

   cout << "A =\n" << A << endl;
   cout << "B =\n" << B << endl;
   cout << "tr(A) = " << A.trace() << endl;
   cout << "tr(B) = " << B.trace() << endl;
   cout << "tr(AB) = " << (A*B).trace() << endl;
   cout << "tr(BA) = " << (B*A).trace() << endl;
   cout << "tr(A)tr(B) = " << A.trace() * B.trace() << endl;

   return 0;
}
