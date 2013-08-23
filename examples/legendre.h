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


// legendre.h
// Recursion formula (n+1)P_{n+1}(x) = (2n+1)xP_n(x)-nP_{n-1}(x)

#include <iostream>
#include <assert.h>
#include "symbolicc++.h"
using namespace std;

class Legendre
{
 private:
   int maxTerm, currentStep;
   Symbolic P, Q;
   const Symbolic &x;

 public:
   Legendre(int,const Symbolic&);
   int step();
   void run();
   void reset();
   Symbolic current() const;
   Symbolic operator () (int);
   friend ostream & operator << (ostream &,const Legendre&);
};

Legendre::Legendre(int n, const Symbolic &kernal) 
   : maxTerm(n), currentStep(0), P(1), x(kernal) {}

int Legendre::step()
{
   int prev = currentStep;
   Symbolic R, one(1);
   ++currentStep;
   if(currentStep==1) { Q = one; P = x; return 1; }
   if(currentStep <= maxTerm)
   {
      R = (prev+currentStep)*x*P/currentStep-prev*Q/currentStep;
      Q = P; P = R;
      return 1;
   }
   return 0;
}

void Legendre::run() { while(step()); }

void Legendre::reset() { currentStep = 0; P = 1; }

Symbolic Legendre::current() const { return P; }

Symbolic Legendre::operator () (int m)
{
   assert(m <= maxTerm);
   reset();
   for(int i=0;i<m;i++) step();
   return P;
}

ostream & operator << (ostream &s,const Legendre &L)
{ return s << L.P; }
