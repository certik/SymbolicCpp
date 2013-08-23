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


// asslegendre.h
// Associate Legendre Polynomial
// P_l^|m|(w) = (1-w^2)^(|m|/2) (d/dw)^|m| P_l(w)
// Recursion formula (n+1)P_{n+1}(x) = (2n+1)xP_n(x)-nP_{n-1}(x)

#include <iostream>
#include "symbolicc++.h"
#include "legendre.h"
using namespace std;

class AssLegendre
{
private:
   Symbolic P;
   const Symbolic &x;

public:
   AssLegendre(const Symbolic &);
   AssLegendre(int,int,const Symbolic &);

   void redefine(int,int);
   Symbolic current() const;

   friend ostream &operator << (ostream &,const AssLegendre &);
};

AssLegendre::AssLegendre(const Symbolic &kernal) : x(kernal) {}

AssLegendre::AssLegendre(int l,int m,const Symbolic &kernal) : x(kernal)
{
   int i, absm = abs(m);
   Legendre L(l,x);
   P = L(l);
   for(i=0;i<absm;i++) P = df(P,x);
   P *= (1 - x*x)^(absm/Symbolic(2)); 
}

void AssLegendre::redefine(int l,int m)
{
   int absm = abs(m);
   Legendre L(l,x);
   P = L(l);
   for(int i=0;i<absm;i++) P = df(P,x);
   P *= (1 - x*x)^(absm/Symbolic(2)); 
}

Symbolic AssLegendre::current() const {  return P; }

ostream & operator << (ostream & s,const AssLegendre &L)
{  return s << L.P; }
