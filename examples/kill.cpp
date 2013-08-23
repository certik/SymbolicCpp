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


// kill.cpp

#include <iostream>
#include "symbolicc++.h"
using namespace std;

int main(void)
{
   int N=4;
   Symbolic g("g",N,N), Lg("Lg",N,N), V("V",N), x("x",N);

   // The Goedel metric
   g(0,0) = 1; g(0,1) = 0;              g(0,2) = 0; g(0,3) = 0;
   g(1,0) = 0; g(1,1) = -exp(2*x(0))/2; g(1,2) = 0; g(1,3) = -exp(x(0));
   g(2,0) = 0; g(2,1) = 0;              g(2,2) = 1; g(2,3) = 0;
   g(3,0) = 0; g(3,1) = -exp(x(0));     g(3,2) = 0; g(3,3) = -1;

   // The Killing vector field of the Goedel metric
   V(0) = x(1); V(1) = exp(-2*x(0)) - x(1)*x(1)/2; 
   V(2) = 0;    V(3) = -2*exp(-x(0));

   // The Lie derivative
   for(int j=0;j<N;j++)
      for(int k=0;k<N;k++)
      {
         Lg(j,k) = 0;
         for(int l=0;l<N;l++)
            Lg(j,k) += V(l)*df(g(j,k),x(l)) + g(l,k)*df(V(l),x(j)) 
                      + g(j,l)*df(V(l),x(k));
      }

   cout << "The Goedel Metric, g\n" << g << endl;
   cout << "The Killing vector field of the Goedel metric, V\n" 
        << V << endl; cout << endl;
   cout << "The Lie derivative of g with respect to V, Lg\n" 
        << Lg << endl;
   return 0;
}
