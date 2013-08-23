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


// pexample3.cpp

#include <iostream>
#include "multinomial.h"
using namespace std;

int main(void)
{
 Multinomial<double> x("x");
 Multinomial<double> y("y");

 Multinomial<double> p=(x^3)+2.0*(x^2)+7.0;

 cout << "p(x)      = " << p <<endl;
 cout << "Diff(p,x) = " << Diff(p,"x") << endl;
 cout << "p(x)^2    = " << (p^2) << endl << endl;

 // multivariable polynomial differentiation 
 Multinomial<double> q=p+(4.0*p)*(y^2);
 cout << "q(x,y)    = " << q << endl;
 cout << "Diff(q,y) = " << Diff(q,"y") << endl;
 cout << "Diff(q,x) = " << Diff(q,"x") << endl;
 cout << "q(y)^2    = " << (q^2) << endl;
 return 0;
}
