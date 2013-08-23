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


// stringexpr.cpp

#include <iostream>
#include <map>
#include <string>
using namespace std;

int main(void)
{
  map<string,string> simplify;
  map<string,string>::iterator i;
  int count = 1;

  simplify["sin(0)"] = "0";
  simplify["cos(0)"] = "1";
  simplify["1*"]     = "";
  simplify["*1"]     = "";
  simplify["+0+"]    = "+";

  string exp = "a*b*cos(0)+sin(0)*c-d+cos(0)*sin(0)+2*d*1";
  cout << "exp = " << exp << endl;

  while(count)
   for(count=0,i=simplify.begin();i!=simplify.end();i++)
   {
    string::size_type pos = exp.find(i->first,0);
    while(pos < exp.npos)
    {
     count++;
     exp.replace(pos,i->first.length(),i->second);
     pos = exp.find(i->first,0);
     cout.setf(ios_base::left,ios_base::adjustfield);
     cout.width(50);
     cout << "exp = " + exp;
     if(i->second == "")
      cout << "(delete " + i->first + ")" << endl;
     else
      cout << "(" + i->first + " -> " + i->second + ")" << endl;
    }
   }
  return 0;
}
