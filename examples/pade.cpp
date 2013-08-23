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


// pade.cpp

#include <iostream>
#include "symbolicc++.h"
using namespace std;

Symbolic Taylor(Symbolic u,Symbolic &x,int n)
{
   Symbolic series = u[x==0];
   int fac = 1;

   for(int j=1;j<=n;j++)
   {
      u = df(u,x); fac = fac*j;
      series += u[x==0]/fac*(x^j);
   }
   return series;
}

Symbolic Pade(const Symbolic &f,Symbolic &x,int N,int M)
{
   int i, j, k, N1 = N+1, M1 = M+1, n = M + N1;
   Symbolic y, z;
   Symbolic a("a",n);
   Symbolic P(Symbolic(0),N1,N1), Q(Symbolic(0),N1,N1);

   y = Taylor(f,x,n);
   for(i=0;i<n;i++) a(i) = y.coeff(x,i);

   for(i=0;i<N;i++)
      for(j=0;j<N1;j++)
      {
         k = M-N+i+j+1;
         if(k >= 0) P(i,j) = Q(i,j) = a(k);
         else       P(i,j) = Q(i,j) = 0;
      }

   for(i=0;i<N1;i++)
   {
      for(j=N-i;j<M1;j++)
      {
         k = j-N+i;
         if(k >= 0) P(N,i) += a(k)*(x^j);
      }
      Q(N,i) = x^(N-i);
   }
   y = det(P); z = det(Q);
   return y/z;
}

int main(void)
{
   Symbolic x("x"), f;
   f = sin(x);
   cout << Pade(f,x,1,1) << endl; // => x
   cout << Pade(f,x,2,2) << endl; // => -1/6*x*(-1/6-1/36*x^2)^(-1)
   cout << Pade(f,x,3,3) << endl; 
        // => (-7/2160*x+1589885/3306816*x^3)*(-7/2160-7/43200*x^2)^(-1)
   return 0;
}
