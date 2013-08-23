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


// lagrange.cpp

#include <iostream>
#include "symbolicc++.h"
using namespace std;

int main(void)
{
 Symbolic x("x"), y("y"), dx("dx"), dy("dy");
 Symbolic f = 2*x*x+y*y;
 Symbolic g = x+y-1;

 // noncommutative
 dx = ~dx; dy = ~dy;

 cout << "f = " << f << endl;
 cout << "g = " << g << endl;
 Symbolic d_f = df(f,x)*dx+df(f,y)*dy;
 Symbolic d_g = df(g,x)*dx+df(g,y)*dy;
 cout << "d_f = " << d_f << endl;
 cout << "d_g = " << d_g << endl;
 Symbolic wedge = (d_f*d_g).subst_all((dx*dx==0,dy*dy==0,dy*dx==-dx*dy));
 cout << (wedge.coeff(dx*dy)==0) << endl;
 return 0;
}

