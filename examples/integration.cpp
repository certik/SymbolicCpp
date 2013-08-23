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


// integration.cpp

#include <iostream>
#include "symbolicc++.h"
using namespace std;

int main(void)
{
  Symbolic x("x"), c("c"), z("z"), y;

  z = z[x];
  y = (1-x)*(1-x)+cos(x)+x*exp(x)+c+z;

  cout << "y = " << y << endl;

  for(int i=0;i<3;i++)
  {
   y = integrate(y,x);
   y = y[integrate(x*exp(x),x) == x*exp(x) - exp(x)];
   cout << "y = " << y << endl;
  }

  return 0; 
}
