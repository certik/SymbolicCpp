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


// tokenizer2.cpp

#include <iostream>
#include <string>
using namespace std;

class StringTokenizer
{
  public:
    StringTokenizer(string s,string d=string(" \t"))
    { toTokenize = s; delimiters = d; first = 0; }
        
    string nextToken()
    {
     if(first == string::npos) return "";
     string::size_type next = toTokenize.find_first_of(delimiters,first);
     string token = toTokenize.substr(first,next-first);
     first = toTokenize.find_first_not_of(delimiters,next);
     return token;
    }

  private:
    string toTokenize, delimiters;
    string::size_type first;
};

int main(void)
{
  string s = "The quick brown fox";
  StringTokenizer st0(s);

  cout << st0.nextToken() << endl; cout << st0.nextToken() << endl;
  cout << st0.nextToken() << endl; cout << st0.nextToken() << endl;

  // try two StringTokenizers non-interleaved
  StringTokenizer st1(string("This is a test"));
  StringTokenizer st2(string("Now is the time"));

  cout << "non-interleaved:" << endl;
  cout << st1.nextToken() << endl; cout << st1.nextToken() << endl;
  cout << st1.nextToken() << endl; cout << st1.nextToken() << endl;
  cout << st2.nextToken() << endl; cout << st2.nextToken() << endl;
  cout << st2.nextToken() << endl; cout << st2.nextToken() << endl;

  // now try two StringTokenizers interleaved
  StringTokenizer st3(string("This is a test"));
  StringTokenizer st4(string("Now is the time"));

  cout << "interleaved:" << endl;
  cout << st3.nextToken() << endl; cout << st4.nextToken() << endl;
  cout << st3.nextToken() << endl; cout << st4.nextToken() << endl;
  cout << st3.nextToken() << endl; cout << st4.nextToken() << endl;
  cout << st3.nextToken() << endl; cout << st4.nextToken() << endl; 
  return 0;
}
