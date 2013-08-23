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


// first2.cpp   

#include <iostream>   
#include "symbolicc++.h"   
using namespace std;

int main(void)   
{      
  Symbolic u("u",3), v("v",4);      
  Symbolic term, sum, I, R1, R2;
  Symbolic t("t"), s("s"), b("b"), r("r");      

  // Lorenz Model      
  v(0) = s*u(1)-s*u(0);     
  v(1) = -u(1)-u(0)*u(2)+r*u(0);      
  v(2) = u(0)*u(1)-b*u(2);
  v(3) = 1;      

  // The ansatz      
  I = (u(1)*u(1)+u(2)*u(2))*exp(2*t);      
  sum = 0;      

  for(int i=0;i<3;i++) sum += v(i)*df(I,u(i));      

  sum += v(3)*df(I,t);      
  cout << "sum = " << sum << endl; cout << endl;      
  R1 = sum.coeff(u(2),2);      
  R1 = R1/(exp(2*t));      
  cout << "R1 = " << R1 << endl;      

  R2 = sum.coeff(u(0),1); R2 = R2.coeff(u(1),1);      
  R2 = R2/(exp(2*t));      
  cout << "R2 = " << R2 << endl;
  return 0;
}
