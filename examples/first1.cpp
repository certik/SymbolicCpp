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


// first1.cpp

#include <iostream>
#include "symbolicc++.h"
using namespace std;

int main(void)
{
   int N=3;
   Symbolic term, sum=0, u("u",N), v("",N), I("",N);

   v(0) = u(1)*u(2); v(1) = u(0)*u(2); v(2) = u(0)*u(1);
   I(0) = u(0)*u(0)-u(1)*u(1);
   I(1) = u(2)*u(2)-u(0)*u(0);
   I(2) = u(1)*u(1)-u(2)*u(2);

   for(int i=0;i<N;i++)
   {
      for(int j=0;j<N;j++)
      {
         term = df(I(i),u(j)) * v(j);
         sum += term;
         cout << "Partial Term " << j << " = " << term << endl;
      }
      cout << "Sum = " << sum << endl;
      cout << endl;
   }
   return 0;
}
