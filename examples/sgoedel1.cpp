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


// sgoedel1.cpp

#include <iostream>
#include "goedel.h"
using namespace std;

int main(void)
{
   Goedel g("acbc");

   cout << "The word is " << g.word();
   cout << " and its corresponding Goedel number is "
        << g.number() << endl;

   g.rename("aabb");

   cout << "The word is " << g.word();
   cout << " and its corresponding Goedel number is "
        << g.number() << endl;

   g.rename("abcbc");

   cout << "The word is " << g.word();
   cout << " and its corresponding Goedel number is "
        << g.number() << endl;
   cout << endl;

   Goedel h;

   // List all the Goedel Numbers below 1500
   for(int i=0;i<1500;i++)
   {
      h.resize(i);
      if(h.is_goedel())
         cout << i << " => " << h.word()
              << " is a Goedel number" << endl;
   }
   return 0;
}
