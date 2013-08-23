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


// picard.cpp

#include <iostream>
#include <string>
#include "polynomial.h"
#include "rational.h"
using namespace std;

int main(void)
{
 Polynomial<Rational<Verylong> > x("x");
 Polynomial<Rational<Verylong> > pic(x);
 Rational<Verylong> zero(string("0")), one(string("1")), two(string("2"));
 int i;

 cout << endl << "x+y up to fifth approximation :" << endl;
 pic = one;
 cout << pic << endl;
 for(i=1;i<=5;i++)
 {
  //integrate and evaluate at the boundaries x and zero
  pic = one + Int(x+pic,"x") - (Int(x+pic,"x"))(zero);
  cout << pic << endl;
 }
 cout << "The approximation at x=2 gives " << pic(two) << endl;

 cout << endl << "x+y^2 up to fourth approximation :" << endl;
 pic = one;
 cout << pic << endl;
 for(i=1;i<=4;i++)
 {
  //integrate and evaluate at the boundaries x and zero
  pic = one + Int(x+(pic^2),"x") - (Int(x+(pic^2),"x"))(zero);
  cout << pic << endl;
 }
 cout << "The approximation at x=2 gives " << pic(two) << endl;

 return 0;
}
