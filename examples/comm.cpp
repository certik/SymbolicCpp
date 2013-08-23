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


// comm.cpp

#include <iostream>
#include "symbolicc++.h"
using namespace std;

const int n = 3;

Symbolic x("x",n);

Symbolic commutator(const Symbolic &V,const Symbolic &W)
{
   Symbolic U(Symbolic(0),V.rows());

   for(int k=0;k<n;k++)
      for(int j=0;j<n;j++)
         U(k) += (V(j)*df(W(k),x(j))-W(j)*df(V(k),x(j)));

   return U;
}

int main(void)
{
   Symbolic V("",n), W("",n), U("",n), Y("",n);

   V(0) = x(0)*x(0); V(1) = x(1)*x(2); V(2) = x(2)*x(2);
   W(0) = x(2);      W(1) = x(0)-x(1); W(2) = x(1)*x(2);
   U(0) = x(1)*x(0); U(1) = x(1);      U(2) = x(0)-x(2);

   // [V,W] = -[W,V]
   Y = commutator(V,W) + commutator(W,V); cout << Y << endl;
   cout << endl;

   // [V,U+W] = [V,U] + [V,W]
   Y = commutator(V,U+W) - (commutator(V,U)+commutator(V,W));
   cout << Y << endl; cout << endl;

   // Jacobian Identity [[V,W],U] + [[U,V],W] + [[W,U],V] = 0
   Y =   commutator(commutator(V,W),U)
       + commutator(commutator(U,V),W)
       + commutator(commutator(W,U),V); 
   cout << Y << endl;
   return 0;
}
