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


// hilbert.cpp

#include <iostream>
#include "matrix.h"
#include "matnorm.h"
#include "symbolicc++.h"
using namespace std;

int main(void)
{
   int n = 2;
   Symbolic a("a"), b("b"), y1;
   Matrix<Symbolic> A(n,n);
   A[0][0] = a; A[0][1] = b;
   A[1][0] = b; A[1][1] = a;

   cout << "The " << n << "x" << n << " matrix A is \n" << A << endl;

   y1 = normH(A);
   cout << "The Hilbert-Schmidt norm of matrix A is " << y1 << endl;
   cout << endl;

   //a = 2.0; b = 3.0;
   cout << "Put a = " << 2.0 << " and b = " << 3.0 << endl;
   cout << "The Hilbert-Schmidt norm of matrix A is "
        << y1[a==2,b==3] << " or " 
        << y1[a==2.0,b==3.0] << endl;
   return 0;
}
