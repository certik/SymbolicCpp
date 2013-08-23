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


// nambu.cpp

#include <iostream>
#include "symbolicc++.h"
using namespace std;

void nambu(const Symbolic &I,const Symbolic &u,int n)
{
   int i, j;
   Symbolic J("J",n,n);

   for(i=0;i<n;i++)
      for(j=1;j<n;j++)
         J(i,j) = df(I(j-1),u(i));

   for(i=0;i<n;i++)
   {
      for(j=0;j<n;j++) J(j,0) = df(u(i),u(j));
      cout << "du(" << i << ")/dt = " << det(J) << endl;
   }
}

int main(void)
{
   Symbolic u("u",3), I("I",2);
   I(0) = u(0)+u(1)+u(2);
   I(1) = u(0)*u(1)*u(2);
   cout << "The equations are " << endl;
   nambu(I,u,3);

   return 0;
}
