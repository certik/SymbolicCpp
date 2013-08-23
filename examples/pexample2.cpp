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


// pexample2.cpp

#include <iostream>
#include "polynomial.h"
#include "rational.h"
using namespace std;

int main(void)
{
 Polynomial<double> x("x");
 Polynomial<Polynomial<double> > y("y"); //multivariable term

 Polynomial<double> p=(x^3)+2.0*(x^2)+7.0;

 cout << "p(x) = " << p <<endl;
 cout << "Diff(p) = " << Diff(p,"x") << endl;
 cout << "Int(p) = " << Int(p,"x") << endl;
 cout << "p(x)^2 = " << (p^2) << endl << endl;

 // multivariable polynomial
 // differentiation and integration are with respect to y
 Polynomial<Polynomial<double> > q=p+(4.0*p)*(y^2);
 cout << "q(y) = " << q << endl;
 cout << "Diff(q,y) = " << Diff(q,"y") << endl;
 cout << "Diff(q,x) = " << Diff(q,"x") << endl;
 cout << "Int(q,y) = " << Int(q,"y") << endl;
 cout << "Int(q,x) = " << Int(q,"x") << endl;
 cout << "q(y)^2 = " << (q^2) << endl;
 cout << "gcd((x^2)-1.0,(x^2)-2.0*x+1.0) = "
      << x.gcd((x^2)-1.0,(x^2)-2.0*x+1.0) << endl;
 Polynomial<Rational<int> > t("t");
 cout << "gcd(..) = "
      << t.gcd(Rational<int>(48)*(t^3)-Rational<int>(84)*(t^2)
               +Rational<int>(42)*t-Rational<int>(36),
               Rational<int>(-4)*(t^3)-Rational<int>(10)*(t^2)
               +Rational<int>(44)*t-Rational<int>(30)) << endl;
 list<Polynomial<Rational<int> > > l =
  (Rational<int>(5)*(t^8)-Rational<int>(10)*(t^6)+Rational<int>(10)*(t^2)
                         -Rational<int>(5)).squarefree();
 cout << "squarefree(...) = " << l.front(); l.pop_front();
 for(int i=1;!l.empty();i++)
 {
  cout << " * (" << l.front() << ")^" << i;
  l.pop_front();
 }
 cout << endl;
 return 0;
}
