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


// laguerre.cpp
// Recursion formula L_{n+1}(x) = (2n+1-x)L_n(x)-n^2L_{n-1}(x)

#include <iostream>
#include "symbolicc++.h"
using namespace std;

class Laguerre
{
private:
   int maxTerm, currentStep;
   Symbolic P, Q;
   const Symbolic &x;

public:
   Laguerre(int,const Symbolic&);

   int step();
   void run();
   Symbolic current() const;
   friend ostream & operator << (ostream&,const Laguerre&);
};

Laguerre::Laguerre(int n,const Symbolic &kernal) 
   : maxTerm(n), currentStep(0), P(1), x(kernal) {}

int Laguerre::step()
{
   int prev = currentStep;
   Symbolic R;
   ++currentStep;
   if(currentStep==1) { Q = 1; P = 1-x; return 1; }
   if(currentStep <= maxTerm)
   {
      R = (2*prev+1-x)*P-prev*prev*Q;
      Q = P; P = R;
      return 1;
   }
   return 0;
}

void Laguerre::run() {  while(step()) ; }

Symbolic Laguerre::current() const { return P; }

ostream & operator << (ostream & s, const Laguerre & L)
{  return s << L.P; }

int main(void)
{
   int n=4;
   Symbolic x("x");
   Laguerre L(n,x);

   // Calculate the first few Laguerre polynomials
   cout << "L(0) = " << L << endl;

   for(int i=1;i<=n;i++)
   {
      L.step();
      cout << "L("<< i << ") = " << L << endl;
   }
   cout << endl;

   // Show that the Laguerre differential equation is satisfied for n = 4.
   Symbolic result;
   result = x*df(L.current(),x,2) + (1-x)*df(L.current(),x)+n*L.current();
   cout << result << endl;
   return 0;
}
