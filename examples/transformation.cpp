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


// transformation.cpp

#include <algorithm>
#include <cctype>
#include <iostream>
#include <string>
#include <vector>
using namespace std;

double cube(double x) { return x*x*x; }

string concat(string s) { return s + "x"; }

char lowercase(char c) { return char(tolower(int(c))); }

int main(void)
{
 vector<double> v;
 v.push_back(0.5);
 v.push_back(1.0);
 v.push_back(1.5);
 transform(v.begin(),v.end(),v.begin(),cube);
 for(int i=0;i<v.size();i++)
  cout << "v[" << i << "] = " << v[i] << endl;

 vector<string> s;
 s.push_back("a");
 s.push_back("ab");
 s.push_back("aba");
 transform(s.begin(),s.end(),s.begin(),concat);
 for(int j=0;j<s.size();j++)
  cout << "s[" << j << "] = " << s[j] << endl;

 string str = "lower CASE";
 transform(str.begin(),str.end(),str.begin(),lowercase);
 cout << "str = " << str << endl;
 return 0;
}
