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


// elliptic.cpp

#include <iostream>
#include "symbolicc++.h"
using namespace std;

int main(void)
{
 Symbolic cn("cn"), sn("sn"), dn("dn"), k("k"), x("x"), x0("x0"), a("a"),
          lambda("lambda"), i = ("i");

 a = a[x];

 Symbolic u = i*lambda*cn[a,k];

 Symbolic res1 = df(u, x, 2);

 res1 = res1.subst_all(
                ( df(sn[a,k], a) ==  cn[a,k] * dn[a,k],
                  df(cn[a,k], a) == -sn[a,k] * dn[a,k],
                  df(dn[a,k], a) == -(k^2) * sn[a,k] * cn[a,k] ) );

 cout << res1 << endl;

 Symbolic res2 = res1 - (u^3);

 Symbolic res3 = res2.subst_all(
     (cn[a,k]^2) == (-1 + 2*(k^2) - (k^2)*(sn[a,k]^2) + (dn[a,k]^2))/(2*k*k) );

 Symbolic res4 = res3[ a == lambda*(x-x0),
                       k == 1/sqrt(Symbolic(2)),
                       (i^3) == -i ];
 cout << res4 << endl;
 return 0;
}
