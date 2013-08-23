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


// squater.cpp

#include <iostream>
#include "quatern.h"
using namespace std;

int main(void)
{
   Quaternion<double> Q1(3,4,5,6),
                      Q2 = Q1.conjugate(),
                      Q3 = Q1.inverse();
   double Mag  = Q1.magnitude(),
          Magz = (~Q1).magnitude();

   cout << "Q1 = "                    << Q1 << endl;
   cout << "Q2 = Conjugate of Q1 = "  << Q2 << endl;
   cout << "Q3 = Inverse of Q1 = "    << Q3 << endl;
   cout << "Mag  = Magnitude of Q1 = " << Mag << endl;
   cout << "Magz = Magnitude of normalized Q1 = " << Magz << endl;
   cout << endl;

   cout << "Q1 * Q2 = " << Q1 * Q2 << endl;
   cout << "Q2 * Q1 = " << Q2 * Q1 << endl;
   cout << "Mag^2 = Square of magnitude = " << Mag * Mag << endl;
   cout << endl;

   cout << "Q1 * Q3 = " << Q1 * Q3 << endl;
   cout << "Q3 * Q1 = " << Q3 * Q1 << endl;

   return 0;
}
