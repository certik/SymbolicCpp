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


// bose.cpp

#include <iostream>
#include "symbolicc++.h"
using namespace std;

int C(int n, int r)
{
 int num = 1, den = 1;
 for(int i=n;i>r;i--) num *= i;
 for(int j=2;j<=(n-r);j++) den *= j;
 return num/den;
}

int main(void)
{
 Symbolic b("b"), bd("b'");
 b = ~b; bd = ~bd;

 for(int m=0; m<5; m++)
 {
  int fac = 1, j;
  Symbolic comm = (b^m)*(bd^m)-(bd^m)*(b^m);
  Symbolic res, res2;
  cout << m << ": " << comm << " = "
       << comm.subst_all(b*bd==1+bd*b) << endl;

  for(j=1; j<=m; j++)
  {
   fac *= j;
   res += df(bd^m,bd,j)*df(b^m,b,j)/fac;
   res2 += fac*C(m,j)*C(m,j)*(bd^(m-j))*(b^(m-j));
  }
  cout << "   " << comm << " = " << res << endl;
  cout << "   " << comm << " = " << res2 << endl;
 }
 return 0;
}
