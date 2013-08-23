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


// hermite.cpp
// Recursion formula H_{n+1}(x) = 2xH_n(x)-2nH_{n-1}(x)

#include <iostream>
#include "symbolicc++.h"
using namespace std;

class Hermite
{
 private:
   int maxTerm, currentStep;
   Symbolic P, Q;
   const Symbolic &x;
 public:
   Hermite(int,const Symbolic&);
   int step();
   void run();
   Symbolic current() const;
   friend ostream & operator << (ostream&,const Hermite&);
};

Hermite::Hermite(int n,const Symbolic &kernal) 
   : maxTerm(n), currentStep(0), P(1), x(kernal) {}

int Hermite::step()
{
   int prev = currentStep;
   Symbolic R;
   ++currentStep;

   if(currentStep==1)
   { Q = 1; P = 2*x; return 1; }

   if(currentStep <= maxTerm)
   {
      R = 2*x*P-2*prev*Q;
      Q = P;
      P = R;
      return 1;
   }
   return 0;
}

void Hermite::run() { while(step()); }

Symbolic Hermite::current() const { return P; }

ostream & operator << (ostream &s,const Hermite &H)
{ return s << H.P; }

int main(void)
{
  int n=4;
  Symbolic x("x");
  Hermite H(n,x);

  // Calculate the first few Hermite polynomials
  cout << "H(0) = " << H << endl;
  for(int i=1;i<=n;i++)
  {
     H.step();
     cout << "H("<< i << ") = " << H << endl;
  }
  cout << endl;

  // Show that the Hermite differential equation is satisfied for n=4.
  Symbolic result;

  result = df(H.current(),x,2)-2*x*df(H.current(),x)+2*n*H.current();

  cout << result << endl;
  return 0;
}
