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


// chebyshev.cpp
// Recursion formula T_{n+1}(x) = 2xT_n(x)-T_{n-1}(x)

#include <iostream>
#include "symbolicc++.h"
using namespace std;

class Chebyshev
{
private:
   int maxTerm, currentStep;
   Symbolic P, Q;
   const Symbolic &x;
public:
   Chebyshev(int,const Symbolic&);
   int step();
   void run();
   Symbolic current() const;
   friend ostream & operator << (ostream&,const Chebyshev&);
};

Chebyshev::Chebyshev(int n,const Symbolic &kernal) 
   : maxTerm(n), currentStep(0), P(1), x(kernal) {}

int Chebyshev::step()
{
 Symbolic R;
 ++currentStep;

 if(currentStep==1) { Q = 1; P = x; return 1; }

 if(currentStep <= maxTerm)
 {
   R = 2*x*P-Q; Q = P; P = R; return 1;
 }
 return 0;
}

void Chebyshev::run() { while(step()); }

Symbolic Chebyshev::current() const { return P; }

ostream & operator << (ostream &s,const Chebyshev &T)
{ return s << T.P; }

int main(void)
{
 int n=4;
 Symbolic x("x");
 Chebyshev T(n,x);

 // Calculate the first few Chebyshev polynomials
 cout << "T(0) = " << T << endl;
 for(int i=1;i<=n;i++)
 {
  T.step();
  cout << "T("<< i << ") = " << T << endl;
 }
 cout << endl;

 // Show that the Chebyshev differential equation is satisfiedi for n=4.
 Symbolic result;

 result = (1-x*x)*df(T.current(),x,2)-x*df(T.current(),x)+n*n*T.current();
 cout << result << endl;
 return 0;
}
