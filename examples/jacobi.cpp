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


// jacobi.cpp

#include <cmath>
#include <iostream>
using namespace std;

// forward declaration
void scdn(double,double,double,double&,double&,double&); 

int main(void)
{
 double x = 3.14159, k, k2, eps = 0.01, res1, res2, res3;

 //sin,cos,1 of x
 k = 0.0;
 k2 = k*k;
 scdn(x,k2,eps,res1,res2,res3);
 cout << "sin(x) = " << res1 << endl;
 cout << "cos(x) = " << res2 << endl;
 cout << "1(x) = " << res3 << endl;

 //tanh,sech,sech of x
 k = 1.0;
 k2 = k*k;
 scdn(x,k2,eps,res1,res2,res3);
 cout << "tanh(x) = " << res1 << endl;
 cout << "sech(x) = " << res2 << endl;
 cout << "sech(x) = " << res3 << endl;
}

void scdn(double x,double k2,double eps,double &s,double &c,double &d)
{
  if(fabs(x) < eps)
  {
   double x2 = x*x/2.0;
   s = x*(1.0 - (1.0 + k2)*x2/3.0);
   c = 1.0 - x2;
   d = 1.0 - k2*x2;
  }
  else
  {
   double sh,ch,dh;

   scdn(x/2.0,k2,eps,sh,ch,dh);  // recursive call

   double sh2 = sh*sh;
   double sh4 = k2*sh2*sh2;
   double denom = 1.0 - sh4;

   s = 2.0*sh*ch*dh/denom;
   c = (1.0 - 2.0*sh2+sh4)/denom;
   d = (1.0 - 2.0*k2*sh2+sh4)/denom;
  }
}
