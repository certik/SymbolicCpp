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


// tokenizer.cpp

#include <iostream>
#include <string>
#include <vector>
using namespace std;

vector<string>* split(string s,char sep)
{
  // dynamically allocate the result vector
  // so that the returning pointer will be valid
  vector<string>* results = new vector<string>;
  string t = "";
  int c, slength;
  slength = s.size();
  // iterate through string s character by character
  for(c=0;c<slength;c++)
   if(s[c] == sep) 
   {
    // end of token found, add to result vector
    results -> push_back(t);  
    t = "";  // start new token
   }
   else { t += s[c]; }  // add char to token
  if(t != "") results -> push_back(t); // get last token
  return results;          // return pointer to result vector
}  // end split function

int main(void)
{
  // pointer to a vector of strings
  vector<string>* sp;
  sp = split("a+b;c*d;d/f",';');
  int vsize = sp -> size();
  cout << "number of terms: " << vsize << endl;
  // display each substring token one by one
  for(int i=0;i<vsize;i++) cout << sp -> at(i) << endl;
  delete sp;
  return 0;
}
