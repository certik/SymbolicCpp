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


// vdpliapunov.cpp

#include <iostream>
#include <cmath>
#include "symbolicc++.h"
using namespace std;

const int N = 3;

Symbolic u("u",N), ut("ut",N), y("y",N-1), yt("yt",N-1);

// The vector field V
template <class T> T V(const T& ss)
{   
  T sum(0);   
  for(int i=0;i<N;i++) sum += ut(i)*df(ss,u(i));   
  return sum;
}

template <class T> T W(const T& ss)
{   
  T sum(0);   
  for(int i=0;i<(N-1);i++) sum += yt(i)*df(ss,y(i));   
  return sum;
}

int main(void)
{   
  int i, j;   
  Symbolic t("t"), a("a"), k("k"), om("om"), y("y",2), us("us",N), ys("ys",N-1);

  Equations values;

  // Driven van der Pol   
  ut(0) = u(1);   
  ut(1) = a*(1 - u(0)*u(0))*u(1) - u(0) + k*cos(u(2));   
  ut(2) = om;   

  // variational equations
  yt(0) = y(1);
  yt(1) = -(2.0*u(0)*u(1) + 1.0)*y(0) - a*(u(0)*u(0) - 1.0)*y(1);

  // Taylor series expansion up to order 2   
  for(i=0;i<N;i++)      
  us(i) = u(i) + t*V(u(i)) + t*t*V(V(u(i)))/2;   
  for(i=0;i<(N-1);i++)      
  ys(i) = y(i) + t*W(y(i)) + t*t*W(W(y(i)))/2;   

  // Evolution of the approximate solution   
  values = (t == 0.01, a == 5.0, k == 5.0, om == 2.466,
            u(0) == 1.5504, u(1) == -0.71214, u(2) == 0.0,
            y(0) == 0.5, y(1) == 0.5);

  int iter = 50000;
  for(j=0;j<iter;j++)   
  {       
   Equations newvalues = (t == 0.01, a == 5.0, k == 5.0, om == 2.466);
   for(i=0;i<N;i++)
    newvalues = (newvalues, u(i) == us(i)[values]);
   for(i=0;i<N-1;i++)
    newvalues = (newvalues, y(i) == ys(i)[values]);

   values = newvalues;
  } // end for loop j

  double T = 0.01*iter;
  double lambda = log(fabs(double(rhs(values,y(0))))
                     +fabs(double(rhs(values,y(1)))))/T;
  cout << "lambda = " << lambda << endl;
  return 0;
}
