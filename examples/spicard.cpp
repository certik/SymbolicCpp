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


// spicard.cpp

#include <iostream>
#include "symbolicc++.h"
using namespace std;

int main(void)
{
 Symbolic x("x"), pic;
 int i;

 cout << endl << "x+y up to fifth approximation :" << endl;
 pic = 1;
 cout << pic << endl;

 for(i=1;i<=5;i++)
 {
  //integrate and evaluate at the boundaries x and zero
  pic = 1+integrate(x+pic,x)-integrate(x+pic,x)[x==0];
  cout << pic << endl;
 }

 cout << "The approximation at x=2 gives " << pic[x==2]
      << " (" << pic[x==2.0] << ")" << endl;

 cout << endl << "x+y^2 up to fourth approximation :" << endl;
 pic = 1;
 cout << pic << endl;

 for(i=1;i<=4;i++)
 {
  //integrate and evaluate at the boundaries x and zero
  pic = 1+integrate(x+pic*pic,x)-integrate(x+pic*pic,x)[x==0];
  cout << pic << endl;
 }
 cout << "The approximation at x=2 gives " << pic[x==2]
      << " (" << pic[x==2.0] << ")" << endl;

 return 0;
}
