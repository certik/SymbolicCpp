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


// mycomplex.cpp

#include <iostream>
#include <complex>
#include <string>
#include "verylong.h"
using namespace std;

int main(void)
{
  complex<double> z1(1.0,-2.0);
  complex<double> z2(3.0,4.0);
  complex<double> v;
  complex<double> w;
  v = z1 + z2;
  cout << "v = " << v << endl;
  w = z1*z2;
  cout << "w = " << w << endl;

  complex<double> pi(3.141592653589793235360287);
  complex<double> i(0,1);
  cout << exp(pi*i) - 1.0 << endl;

  Verylong two("2");
  Verylong three("3");
  Verylong four("4");
  Verylong five("5");
  complex<Verylong> u1(two,three);
  complex<Verylong> u2(four,five);
  complex<Verylong> u3;
  u3 = u1 + u2;
  cout << "u3 = " << u3 << endl;

  complex<double> a(2.0,3.0);
  complex<double> b(1.0,-4.0);
  complex<complex<double> > c(a,b);
  cout << "c = " << c << endl;
  complex<double> d(-3.5,7.0);
  complex<double> e(2.5,-5.5);
  complex<complex<double> > f(d,e);
  complex<complex<double> > h;
  h = c + f;
  cout << "h = " << h << endl;

  double rparta = a.real();
  cout << "rparta = " << rparta << endl;
  double iparta = a.imag();
  cout << "iparta = " << iparta << endl;

  complex<double> sz = sin(a);
  cout << "sz = " << sz << endl;

  // principle branch
  cout << "log(i) = " << log(i) << endl;
  cout << "i^i = " << pow(i,i) << endl;

  return 0;
}
