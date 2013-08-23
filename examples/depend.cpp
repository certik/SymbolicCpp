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


// depend.cpp

#include <iostream>
#include "symbolicc++.h"
using namespace std;

int main(void)
{
  Symbolic a("a"), b("b"), u("u"), v("v"), x("x"), z("z"), y("y");

  cout << "System assumes no dependency by default" << endl;
  cout << "df(y,x) => " << df(y,x) << endl;

  cout << "y is dependent on x" << endl;
  y = y[x];
  cout << "df(y,x) => " << df(y,x) << endl;
  y = sin(x*x+5) + x; cout << "y = " << y << endl;
  cout << "df(y,x) => " << df(y,x) << endl;
  cout << endl;

  cout << "u depends on x" << endl;
  u = u[x];
  cout << "df(cos(u),x) => " << df(cos(u),x) << endl;

  cout << "u depends on x, and x depends on v" << endl;
  x = x[v];
  u = Symbolic("u")[x];
  cout << "df(cos(u),v) => " << df(cos(u),v) << endl;

  // example
  y = cos(x*x+5)-2*sin(a*z+b*x); cout << "y = " << y << endl;
  cout << "df(y,v) => " << df(y,v) << endl;
  cout << endl;

  cout << "remove dependency" << endl;
  y = Symbolic("y");
  cout << "df(y,v) => " << df(y,v) << endl;
  cout << endl;

  cout << "derivative of constants gives zero" << endl;
  cout << "df(5,x) => " << df(5,x) << endl;
  cout << endl;

  // renew the variable y
  y = Symbolic("y");
  y = y[x, z]; 
  cout << "df(y,x)+df(y,x) => " << df(y,x)+df(y,x) << endl;
  cout << "df(y,x)*df(y,x) => " << df(y,x)*df(y,x) << endl;
  cout << endl;

  cout << "Another example," << endl;
  x = Symbolic("x");
  y = Symbolic("y"); 
  u = Symbolic("u");
  v = v[x];
  u = u[v]; 
  y = y[u]; 
  y = df(u,x)*sin(x);
  cout << "y = " << y << endl;
  cout << "let u = " << 2*v*x << " then," << endl;
  cout << "y => " << y[u == 2*v*x] << endl;
  cout << "df(y[u == 2*v*x],x) => " << df(y[u == 2*v*x],x) << endl;
  return 0;
}
