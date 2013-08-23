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


// isprime.cpp

#include <iostream>
#include "verylong.h"
using namespace std;

template <class T> int is_prime(T p)
{
   T j(2), zero(0), one(1), two(2), four(4);
   T limit = T(sqrt(p)) + one;

   if(j < limit && p%j == zero) return 0;
   j++;

   if(j < limit && p%j == zero) return 0;
   j += two;
   while(j < limit)
   {
      if(p%j == zero) return 0;
      j += two;
      if(p%j == zero) return 0;
      j += four;
   }
   return 1;
}

int main(void)
{
   Verylong x;
   cout << "Please enter a positive integer number : ";
   cin >> x;
   if(is_prime(x)) cout << "The number " << x << " is prime " << endl;
   else            cout << "The number " << x << " is not prime " << endl;
   return 0;
}
