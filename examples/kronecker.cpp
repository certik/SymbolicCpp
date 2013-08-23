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


// kronecker.cpp

#include <iostream>
#include "matrix.h"
using namespace std;

int main(void)
{
   Matrix<int> A(2,3), B(3,2), C(3,1), D(2,2);

   A[0][0] =  2; A[0][1] = -4; A[0][2] = -3;
   A[1][0] =  4; A[1][1] = -1; A[1][2] = -2;
   B[0][0] =  2; B[0][1] = -4;
   B[1][0] =  2; B[1][1] = -3;
   B[2][0] =  3; B[2][1] = -1;
   C[0][0] =  2;
   C[1][0] =  1;
   C[2][0] = -2;
   D[0][0] =  2; D[0][1] =  1;
   D[1][0] =  3; D[1][1] = -1;

   cout << kron(A,B) << endl;
   cout << kron(B,A) << endl;
   cout << kron(A,B)*kron(C,D) - kron(A*C,B*D) << endl;
   cout << dsum(A,B) << endl;
   return 0;
}
