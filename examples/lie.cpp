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


// lie.cpp

#include <iostream>
#include "symbolicc++.h"
using namespace std;

const int N = 3;

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
  int i,j;   
  Symbolic t("t"), s("s"), b("b"), r("r");
  Symbolic us("",N);
  Equations values;

  cout << u << endl;
  // Lorenz model   
  ut(0) = s*(u(1)-u(0));   
  ut(1) = -u(1)-u(0)*u(2)+r*u(0);   
  ut(2) = u(0)*u(1)-b*u(2);   

  // Taylor series expansion up to order 2   
  for(i=0;i<N;i++)      
   us(i) = u(i)+t*V(u(i))+0.5*t*t*V(V(u(i)));   
  cout << "us =\n" << us << endl;   

  // Evolution of the approximate solution   
  values = (t==0.01,r==40.0,s==16.0,b==4.0,u(0)==0.8,u(1)==0.8,u(2)==0.8);   

  for(j=0;j<100;j++)   
  {       
   Equations newvalues = (t==0.01,r==40.0, s==16.0,b==4.0);
   for(i=0;i<N;i++) 
   {
    newvalues = (newvalues,u(i)==us(i)[values]);
    cout << newvalues.back() << endl;
   }
   values = newvalues;
  }
  return 0;
}

