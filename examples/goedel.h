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


// goedel.h

#ifndef GOEDEL_H
#define GOEDEL_H

#include <cassert>
#include <iostream>
#include <string>
#include "prime.h"
#include "verylong.h"
using namespace std;

const size_t max_prime = 100000;

class Goedel
{
 private:
   // Data Fields
   Verylong nvalue;
   string   wvalue;
   int      is_G;

 public:
   // Constructors
   Goedel();
   Goedel(string);
   Goedel(Verylong);

   // Member Functions
   Verylong number() const;
   string   word() const;
   int      is_goedel() const;

   void rename(string);
   void resize(Verylong);
};

Goedel::Goedel()
   : nvalue(Verylong("0")), wvalue(string("")), is_G(1) {}

Goedel::Goedel(string s)
   : nvalue(Verylong("1")), wvalue(s), is_G(1)
{
   static Prime<max_prime> p;
   Verylong temp, prim, zero("0"), one("1");
   p.run();

   for(int i=0;unsigned(i)<s.length();i++)
   {
      if(s[i]>='a' && s[i]<='c')
      {
         temp = one;
         prim = Verylong(p(i));
         for(char j='a';j<=s[i];j++) temp *= prim;
         nvalue *= temp;
      }
      else { nvalue = zero; break; }
   }
}

Goedel::Goedel(Verylong num) : nvalue(num), wvalue(string(""))
{
   static Prime<max_prime> p;
   static Verylong zero("0"), one("1");
   Verylong prim;
   int i=0, factor; 

   p.run();

   while(num>one)
   {
      factor = 0; prim = Verylong(p(i));
      while(num % prim == zero) { num /= prim; ++factor; }
      switch(factor)
      {
         case 1: wvalue = wvalue + string("a"); break;
         case 2: wvalue = wvalue + string("b"); break;
         case 3: wvalue = wvalue + string("c"); break;
         default : is_G = 0; return;
      }
      ++i;
   }
   is_G = 1;
}

Verylong Goedel::number() const { return nvalue; }

string Goedel::word() const
{  
   if(is_G) return wvalue;
   return string("");
}

int Goedel::is_goedel() const { return is_G; }

void Goedel::rename(string s)
{
   static Prime<max_prime> p;
   static Verylong zero("0"), one("1");
   Verylong temp, prim;
   p.run();
   nvalue = one;
   wvalue = s;
   is_G = 1;
   for(int i=0;unsigned(i)<s.length();i++)
   {
      if(s[i]>='a' && s[i]<='c')
      {
         temp = one;
         prim = Verylong(p(i));
         for(char j='a';j<=s[i];j++) temp *= prim;
         nvalue *= temp;
      }
      else { nvalue = zero; break; }
   }
}

void Goedel::resize(Verylong num)
{
   static Verylong zero("0"), one("1");
   assert(num >= zero);
   if(num == one) { is_G = 0; return; }
   static Prime<max_prime> p;
   int i=0, factor;
   Verylong prim;
   p.run();
   nvalue = num; wvalue = string("");
   while(num>one)
   {
      factor = 0; prim = Verylong(p(i));
      while(num % prim == zero) { num /= prim; ++factor; }
      switch(factor)
      {
         case 1: wvalue = wvalue + string("a"); break;
         case 2: wvalue = wvalue + string("b"); break;
         case 3: wvalue = wvalue + string("c"); break;
         default : is_G = 0; return;
      }
      ++i;
   }
   is_G = 1;
}
#endif
