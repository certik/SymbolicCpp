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


// commute.cpp

#include <iostream>
#include "symbolicc++.h"
using namespace std;

int main(void)
{
   Symbolic a("a"), b("b"), y;

   cout << "Commutative Algebra" << endl;
   cout << "===================" << endl;

   // The system is commutative by default
   y = a*b*a;      cout << " y = " << y << endl;
   y = a*b-b*a;    cout << " y = " << y << endl;

   y = ((a-b)^2);  cout << " y = " << y << endl;
   cout << endl;

   cout << "Non Commutative Algebra" << endl;
   cout << "=======================" << endl;
   a = ~a; b = ~b;

   y = a*b*a;      cout << " y = " << y << endl;
   y = a*b-b*a;    cout << " y = " << y << endl;

   y = ((a-b)^2);  cout << " y = " << y << endl;
   cout << endl;

   cout << "Commutative Algebra" << endl;
   cout << "===================" << endl;
   cout << " y = " << y[a == ~a, b == ~b] << endl;

   return 0;
}
