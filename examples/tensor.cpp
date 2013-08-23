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


// tensor.cpp

#include <iostream>
#include "array.h"
#include "symbolicc++.h"
using namespace std;

int main(void)
{ 
   int a,b,m,n,c,K=2;
   Symbolic g("g",K,K), g1("g1",K,K);   // inverse of g
   Array<Symbolic,2> Ricci(K,K), Ricci1(K,K);
   Array<Symbolic,3> gamma(K,K,K); 
   Array<Symbolic,4> R(K,K,K,K);
   Symbolic u("u"), x("x",2);
   Symbolic sum, RR;

   // u depends on x1 and x2
   u = u[x(0),x(1)];

   g(0,0) = 1;       g(0,1) = cos(u);
   g(1,0) = cos(u);  g(1,1) = 1;

   g1 = g.inverse();

   for(a=0;a<K;a++)
      for(m=0;m<K;m++)
         for(n=0;n<K;n++)
         {
            sum = 0;
            for(b=0;b<K;b++)
               sum += g1(a,b)*(df(g(b,m),x(n))+df(g(b,n),x(m))
                      -df(g(m,n),x(b)));
            gamma[a][m][n] = sum / 2;

            cout << "gamma(" << a << "," << m << "," << n << ") = " 
                 << gamma[a][m][n] << endl;
         }
   cout << endl;

   for(a=0;a<K;a++)
      for(m=0;m<K;m++)
         for(n=0;n<K;n++)
            for(b=0;b<K;b++)
            {
               R[a][m][n][b] = df(gamma[a][m][b],x(n))
                               -df(gamma[a][m][n],x(b));

               for(c=0;c<K;c++)
               {
                  R[a][m][n][b] += gamma[a][c][n]*gamma[c][m][b]
                                 - gamma[a][c][b]*gamma[c][m][n];
               }
               R[a][m][n][b] = R[a][m][n][b].subst(cos(u)*cos(u),
                                                   1-sin(u)*sin(u));
            }

   for(m=0;m<K;m++)
      for(n=0;n<K;n++)
      {
         Ricci[m][n] = 0;
         for(b=0;b<K;b++) Ricci[m][n] += R[b][m][b][n];

         cout << "Ricci(" << m << "," << n << ") = "
              << Ricci[m][n] << endl; 
      }
   cout << endl;

   for(m=0;m<K;m++)
      for(n=0;n<K;n++)
      {
         Ricci1[m][n] = 0;
         for(b=0;b<K;b++) Ricci1[m][n] += g1(m,b)*Ricci[n][b];
      }

   RR = 0;
   for(b=0;b<K;b++) RR += Ricci1[b][b];
   RR = RR[cos(u)*cos(u)==1-sin(u)*sin(u)];
   cout << "R = " << RR << endl;
   return 0;
}
