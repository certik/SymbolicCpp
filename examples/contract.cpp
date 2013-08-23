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


// contract.cpp

#include <iostream>
#include "rational.h"
#include "verylong.h"
using namespace std;

class Map
{
private:
   Rational<Verylong> (*function)(Rational<Verylong>);
   Rational<Verylong> value;        // current iterated value

public:
   Map(Rational<Verylong> (*f)(Rational<Verylong>), Rational<Verylong>);
   void operator ++ ();             // next iteration
   void Is_FP();                    // Is there a fixed point?
};

Map::Map(Rational<Verylong> (*f)(Rational<Verylong>),
         Rational<Verylong> x0) : function(f), value(x0) {}

void Map::operator ++ () {  value = (*function)(value); }

// Is there a fixed point?
void Map::Is_FP()
{
   Rational<Verylong> temp,  // the value in previous step
                      dist;  // the relative difference
                             // between previous and current step
   do
   {
      temp = value;
      ++(*this);
      dist = abs((value-temp)/temp); // the relative difference
      cout << "Value : " << value << " or " << double(value) << endl;
      cout << endl;
   } while(double(dist) > 1e-5);
}

// f(x) = 5/2 x(1-x)
Rational<Verylong> mapping(Rational<Verylong> x)
{
   return (Rational<Verylong>("5/2")*x*(Rational<Verylong>("1")-x));
}

int main(void)
{
   // initial value = 9/10
   Map M(mapping, Rational<Verylong>(Verylong("9"),Verylong("10")));
   M.Is_FP();
   return 0;
}
