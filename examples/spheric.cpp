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


// spheric.cpp

#include <iostream>
#include "symbolicc++.h"
#include "asslegendre.h"
using namespace std;

Symbolic PI("PI"), I("I");

int factorial(int n)
{
   int result=1;
   for(int i=2;i<=n;i++) result *= i;
   return result;
}

Symbolic Y(int l,int m,const Symbolic &phi,const Symbolic &w)
{
   Symbolic a, b, u("u"), result;
   int absm = abs(m);
   AssLegendre A(l,m,u);
   a = A.current(); a = a[u == cos(w)];
   if(m>0 && m%2) a = -a;
   b = sqrt((2*l+1)*factorial(l-absm)/(4*factorial(l+absm)*PI));
   result = a*b*exp(I*m*phi);
   return result;
}

int main(void)
{
   int n=3;
   Symbolic phi("phi"), w("w"), result;

   for(int i=0;i<=n;i++)
   {
      for(int j=-i;j<=i;j++)
      {
         result = Y(i,j,phi,w);
         result = result[cos(w)*cos(w) == 1-sin(w)*sin(w)];
         cout << "Y(" << i << "," << j << ") = " << result << endl;
      }
      cout << endl;
   }
   return 0;
}
