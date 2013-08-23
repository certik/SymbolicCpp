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


// convertbinary.cpp

#include <iostream>
#include <string>
#include <cmath>
using namespace std;

unsigned long N(string s)
{
  unsigned long sum = 0, p2=1;
  for(int i=s.length()-1;i>=0;i--,p2*=2) sum += (s[i]-'0')*p2;
  return sum;
}

string Ninv(unsigned long x)
{
  string s = "";  // empty string
  for(;x>0;x/=2) s = char((x%2)+'0')+s;
  return s;
}

unsigned long map(string s)
{
  unsigned long n = N(s);
  if(s.length()>1) n += (1 << s.length())-2;
  return n;
}

string mapinv(unsigned long x)
{
  unsigned long n = (unsigned long)floor(log(x+2.0)/log(2.0));
  string b;
  if(n == 1) b = Ninv(x);
  else b = Ninv(x-(1<<n)+2);
  while(b.length()<n) b = '0' + b;
  return b;
}

int main(void)
{
  string s;
  for(unsigned long i=0;i<20;i++)
  {
   s = mapinv(i);
   cout.width(3); cout << map(s) << " -> " << s << endl;
  }
  return 0;
}
