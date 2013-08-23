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


// division.cpp

#include <iostream>
#include "verylong.h"
using namespace std;

int main(void)
{
  Verylong P("999"), Q("111"), D("105");

  P = pow(P,D);     // 999^105 = 9.00277e+314 (exceeded double limit)
  Q = pow(Q,D);     // 111^105 = 5.74001e+214

  cout << div(P,Q) << endl;             // 1.56842e+100 - OK
  cout << div(Q,P) << endl;             // 6.37583e-101 - OK
  cout << double(P)/double(Q) << endl;  // NaN
  cout << double(Q)/double(P) << endl;  // NaN

  return 0;
}
