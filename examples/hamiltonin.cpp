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


// hamiltonin.cpp

#include <iostream>
#include "symbolicc++.h"
using namespace std;

int main(void)
{
  Symbolic h("h"), q("q",3), p("p",3), pt("pt",3), qt("qt",3),
           I1, R1, I2, R2;

  int j;

  // Hamilton function
  h = (p(0)*p(0)+p(1)*p(1)+p(2)*p(2))/2
     +exp(q(0)-q(1))+exp(q(1)-q(2))+exp(q(2)-q(0));

  for(j=0;j<3;j++)
  {
   pt(j) = -df(h,q(j)); qt(j) = df(h,p(j));
   cout << "dp" << j << "/dt = " << pt(j) << endl;
   cout << "dq" << j << "/dt = " << qt(j) << endl;
  }

  I1 = p(0)+p(1)+p(2);
  R1 = 0;
  for(j=0;j<3;j++) R1 += pt(j)*df(I1,p(j))+qt(j)*df(I1,q(j));
  if(R1 == 0) cout << "I1 is a first integral." <<endl;
  else cout << "I1 is not a first integral." <<endl;

  I2 = p(0)*p(1)*p(2)-p(0)*exp(q(1)-q(2))-p(1)*exp(q(2)-q(0))
      -p(2)*exp(q(0)-q(1));
  R2 = 0;
  for(j=0;j<3;j++) R2 += pt(j)*df(I2,p(j))+qt(j)*df(I2,q(j));
  if(R2 == 0) cout << "I2 is a first integral." << endl;
  else cout << "I2 is not a first integral." << endl;
  return 0;
}

