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


// backlund.cpp

#include <iostream>
#include "symbolicc++.h"
using namespace std;

int main(void)
{
 Symbolic ut("ut"), xt("xt"), x("x"), u("u"), lambda("lambda");
 Symbolic res1, res2, i = sqrt(Number<int>(-1));

 xt = xt[x];
 ut = ut[xt];
 u = u[x];

 res1 = df(u, x) - i*df(ut, xt) - 2*exp(i*lambda)*sin((u+i*ut)/2);
 res2 = df(res1, x);

 res2 = res2[ df(xt, x) == 1 ];

 cout << res2 << endl;
 
 return 0;
}
