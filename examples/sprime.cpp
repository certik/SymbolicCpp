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


// sprime.cpp

#include <iostream>
#include "prime.h"
using namespace std;

int main(void)
{
   const size_t max0 = 1000000;
   unsigned int i, j, count = 0;

   Prime<max0> p;   // specifies the upper limit of the sequence
   p.run();         // generates the prime number sequence

   // for all odd numbers greater than 3, check if they are prime
   for(i=3,j=100;i<=max0;i+=2)
   {
      if(p.is_prime(i)) count++;

      // sum the number of primes below 100, 1000, ..., 1000000
      if(i==j-1)
      {
         j *= 10;
         cout << "There are " << count + 1
              << " primes below " << i+1 << endl;
      }
   }
   cout << endl;

   // print the first 20 primes
   for(i=0;i<20;i++) cout << p(i) << " ";
   cout << endl << endl;

   // randomly pick some primes
   cout << "The   100th prime is " <<  p(99)   << endl;
   cout << "The   200th prime is " <<  p(199)  << endl;
   cout << "The  3000th prime is " <<  p(2999) << endl;
   cout << "The 10000th prime is " <<  p(9999) << endl;
   cout << "The 12500th prime is " << p(12499) << endl;

   return 0;
}
