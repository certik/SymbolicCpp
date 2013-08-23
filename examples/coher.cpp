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


// coher.cpp

#include <iostream>
#include "symbolicc++.h"
using namespace std;

int main(void)
{
 Symbolic b("b"), bd("bd"), cs("cs"), ds("ds"),
          z("z"), w("w"), conj("conj");

 // non-commutative symbols
 b = ~b; bd = ~bd; cs = ~cs; ds = ~ds;

 Equations rules = (    b*cs[z] == z*cs[z],
                        b*cs[w] == w*cs[w],
                    ds[z]*bd    == ds[z]*conj[z],
                    ds[w]*bd    == ds[w]*conj[w],
                    ds[z]*cs[z] == 1,
                    ds[w]*cs[w] == 1,
                    ds[w]*cs[z] == exp(-(z*conj[z]+w*conj[w]-2*conj[w]*z)/2),
                    ds[z]*cs[w] == exp(-(z*conj[z]+w*conj[w]-2*conj[z]*w)/2));

 // Example 1
 Symbolic r1 = b*(b*cs[z]);
 r1 = r1.subst_all(rules);
 cout << r1 << endl;
 r1 = r1[z == 1];
 cout << r1 << endl;

 // Example 2
 cout << (ds[z]*cs[z]).subst_all(rules) << endl;

 // Example 3
 Symbolic r2 = b*cs[z];
 Symbolic r3 = ds[w]*r2;

 cout << r2.subst_all(rules) << endl;
 cout << r3.subst_all(rules) << endl;

 return 0;
}
