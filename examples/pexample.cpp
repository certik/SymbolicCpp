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


// pexample.cpp

#include <iostream>
#include "polynomial.h"
#include "rational.h"
using namespace std;

int main(void)
{
  Polynomial<double> x("x");
  Polynomial<double> p1 = (x^3)+2.0*(x^2)+7.0;
  cout << "p1(x) = " << p1 << endl;
  Polynomial<double> p2 = (x^2) - x - 1.0;
  cout << "p2(x) = " << p2 << endl;
  Polynomial<double> p3 = p1 + 2.0*p2;
  cout << "p3(x) = " << p3 << endl;
  Polynomial<double> p4 = 3.0*p1*p2 + 2.0;
  cout << "p4(x) = " << p4 << endl;
  Polynomial<double> p5 = (x^2)-1.0;
  Polynomial<double> p6 = x-1.0;
  Polynomial<double> p7 = p5/p6;
  cout << "p7(x) = " << p7 << endl;
  return 0;
}
