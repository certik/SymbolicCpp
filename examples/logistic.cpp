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


// logistic.cpp

#include "rational.h"
#include "verylong.h"
using namespace std;

int main(void)
{
   Rational<int> a(4,1), b(1,1), x0(1,3); // initial value x0 = 1/3
   int i;

   cout << "x[0] = " << x0 << " or " << double(x0) << endl;

   for(i=1;i<=4;i++)     // cannot use higher values than 4
   {                     // out of range for data type int
      x0 = a*x0*(b - x0);
      cout << "x[" << i << "] = " << x0 << " or " << double(x0) << endl;
   }
   cout << endl;

   Rational<Verylong> c1("1"), c2("4/1"),
                      y0("1/3");        // initial value y0 = 1/3

   cout << "y[0] = " << y0 << " or " << double(y0) << endl;

   for(i=1;i<=6;i++)
   {
      y0 = c2*y0*(c1 - y0);
      cout << "y[" << i << "] = " << y0 << " or " << double(y0) << endl;
   }

   return 0;
}
