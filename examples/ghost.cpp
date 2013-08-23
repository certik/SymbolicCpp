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


// ghost.cpp

#include <fstream>
#include <math.h>
#include "rational.h"
#include "verylong.h"
using namespace std;

const Rational<Verylong> h("1/10");    // time-mesh length
const Rational<Verylong> x0("99/100"); // initial value

int main(void)
{
   Rational<Verylong> u, v, u1, twoh, t,
                      zero("0"), one("1"), two("2"), fifty("50");
   ofstream sout("ghosts.dat"); // contain the rational number values

   // initial values
   u = x0; v = u + h*u*(one-u);
   t = zero; twoh = two*h;

   while(t <= fifty)
   {
      u1 = u; u = v;
      v = u1 + twoh*u*(one-u);
      t += h;
      sout << t << " " << v << endl;
   }

   return 0;
}
