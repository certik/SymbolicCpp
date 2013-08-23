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


// deter.cpp

#include <iostream>
#include "matrix.h"
using namespace std;

int main(void)
{
   Matrix<double> A(2,2);
   A[0][0] = 1; A[0][1] = 2;
   A[1][0] = 3; A[1][1] = 4;
   cout << A;
   cout << "Determinant of the matrix is " << A.determinant() << endl; 
   cout << endl;

   for(int i=3;i<5;i++)
   {
      A.resize(i,i,i);
      cout << A;
      cout << "Determinant of the matrix is " << A.determinant() << endl; 
      cout << endl;
   }
   return 0;
}
