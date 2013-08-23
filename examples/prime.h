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


// prime.h

#ifndef PRIME_H
#define PRIME_H

#include <cassert>
#include <iostream>
#include <bitset>
using namespace std;

template <size_t n>
class Prime
{
private:
   // Data Fields
   unsigned int max_num, max_index, index, p, q, current;
   bitset<(n+1)/2 - 1> bvec;

public:
   // Constructors
   Prime();
   Prime(unsigned int num);

   void reset();
   int  step();
   void run();
   int  is_prime(unsigned int) const;
   unsigned int current_prime() const;

   unsigned int operator () (unsigned int) const;
};

template <size_t n> Prime<n>::Prime()
   : max_num(n), max_index((n+1)/2 - 1), index(0),
     p(3), q(3), current(2) { bvec.set(); }

template <size_t n> void Prime<n>::reset()
{
   assert(n > 1);
   max_num = n;
   max_index = (n+1)/2 - 1;
   index = 0; p = 3; q = 3; current = 2;
   bvec.set();
}

template <size_t n> int Prime<n>::step()
{
   if(index < max_index)
   {
      while(!bvec.test(index))
      {
         ++index;
         p += 2;
         if(q < max_index) q += p+p-2;
         if(index > max_index) return 0;
      }
      current = p;

      if(q < max_index)
      {
         // cross out all odd multiples of p, starting with p^2
         // k = index of p^2
         unsigned int k = q;
         while(k < max_index) { bvec.reset(k); k += p; }
         ++index;
         p += 2;
         q += p+p - 2;
         return 1;
      }
      // p^2 > n, so bvec has all primes <= n recorded
      else // to next odd number
      {
         p += 2;
         ++index;
         return 2;
      }
   }
   else return 0;
}

template <size_t n> void Prime<n>::run() { while(step() == 1); }

template <size_t n>
int Prime<n>::is_prime(unsigned int num) const
{
   if(!(num%2)) return 0;
   if(bvec.test((num-3)/2)) return 1;
   return 0;
}
   
template <size_t n>
unsigned int Prime<n>::current_prime() const
{  return current; }

template <size_t n> 
unsigned int Prime<n>::operator () (unsigned int idx) const
{
   unsigned int i;
   if(idx == 0) return 2;
   for(i=0;idx && i<max_index;i++)
      if(bvec.test(i)) --idx;
   return 2*i + 1;
}
#endif
