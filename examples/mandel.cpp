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


// mandel.cpp

#include <iostream>
#include <complex>
#include "rational.h"
#include "verylong.h"
using namespace std;

int main(void)
{
   complex<Rational<long> > c(Rational<long>(1,8),Rational<long>(1,3)),
                            z0(Rational<long>(0,1),Rational<long>(0,1));

   cout << "Using data type Rational<long>" << endl;
   for(int i=1;i<=3;i++)
   {
      z0 = z0*z0 + c;
      cout << "z[" << i << "] = " << z0 << endl;
   }
   cout << endl;

   complex<Rational<Verylong> >
      d(Rational<Verylong>(1,8),Rational<Verylong>(1,3)),
      w0(Rational<Verylong>(0,1),Rational<Verylong>(0,1));

   cout << "Using data type Rational<Verylong>" << endl;
   for(int j=1;j<=5;j++)
   {
      w0 = w0*w0 + d;
      cout << "w[" << j << "] = " << w0 << endl;
   }
   return 0;
}
