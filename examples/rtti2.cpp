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


// rtti2.cpp

#include <iostream>
#include <typeinfo>
#include <string>
using namespace std;

class base1
{
 public: string whatami(void) { return typeid(*this).name(); }
};
class derived11: public base1 {};
class derived12: public base1 {};

class base2
{
 public: virtual string whatami(void) { return typeid(*this).name(); }
         virtual ~base2() {}
};
class derived21: public base2 {};
class derived22: public base2 {};

int main(void)
{
   derived11 d11;
   derived12 d12;
   base1*     b1;
   derived21 d21;
   derived22 d22;
   base2*     b2;
   b1 = &d11;
   cout << b1->whatami() << endl;
   b1 = &d12;
   cout << b1->whatami() << endl;
   b2 = &d21;
   cout << b2->whatami() << endl;
   b2 = &d22;
   cout << b2->whatami() << endl;
   return 0;
}
