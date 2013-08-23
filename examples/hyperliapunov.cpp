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


// hyperliapunov.cpp

#include <iostream>
#include <cmath>
#include "symbolicc++.h"
using namespace std;

const int N = 10;

Symbolic u("u",N), ut("ut",N);

// The vector field V
template <class T> T V(const T& ss)
{   
  T sum(0);   
  for(int i=0;i<N;i++) sum += ut(i)*df(ss,u(i));   
  return sum;
}

int main(void)
{   
  int i, j;   
  Symbolic t("t"), us("",N); 

  Equations values;
  double r1 = 1.0/4.0, r2 = 11.0/5.0, r3 = 1.0/20.0, r4 = 3.0/10.0;

  // hyperchaotic model   
  ut(0) = -u(1) - u(2);   
  ut(1) = u(0) + r1*u(1) + u(3);   
  ut(2) = r2 + u(0)*u(2);   
  ut(3) = r3*u(3) - 0.5*u(2);

  ut(4) = r1*u(4) + u(6) + u(7);
  ut(5) = u(0)*u(5) - u(7);
  ut(6) = r3*u(6) - 0.5*u(5) - u(8) - u(9);
  ut(7) = (r1 + u(0))*u(7) - u(2)*u(4) + u(5) - u(9);
  ut(8) = r4*u(8) + u(6) - 0.5*u(7);
  ut(9) = (u(0) + r3)*u(9) + u(2)*u(6);

  // Taylor series expansion up to order 2   
  for(i=0;i<N;i++)      
  us(i) = u(i) + t*V(u(i)) + 0.5*t*t*V(V(u(i)));   

  // Evolution of the approximate solution   
  // initial values
  values = (u(0) == -19.0, u(1) == 0.0, u(2) == 0.0,
            u(3) ==  15.0, u(4) == 1.0, u(5) == 1.0,
            u(6) ==   1.0, u(7) == 1.0, u(8) == 1.0,
            u(9) ==   1.0, t == 0.01);

  int iter = 20000;
  for(j=0;j<iter;j++)   
  {       
   Equations newvalues;
   newvalues = (newvalues, t == 0.01);

   for(i=0;i<N;i++)
    newvalues = (newvalues, u(i) == us(i)[values]);
   values = newvalues;
  } // end for loop j

  double T = double(rhs(values,t))*iter;
  double lambda = 
    log(fabs(double(rhs(values,u(4))))
       +fabs(double(rhs(values,u(5))))
       +fabs(double(rhs(values,u(6))))
       +fabs(double(rhs(values,u(7))))
       +fabs(double(rhs(values,u(8))))
       +fabs(double(rhs(values,u(9)))))/T;
  cout << "lambda = " << lambda << endl;
  return 0;
}

