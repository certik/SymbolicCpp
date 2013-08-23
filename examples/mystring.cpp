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


// mystring.cpp

#include <iostream>
#include <string>
using namespace std;

int main(void)
{
  string str1("The string class provides C++");
  string str2(" with string handling.");
  string str3;

  // assignment
  str3 = str1;
  cout << str3 << endl; // => The string class provides C++

  // comparison
  int result = (str1 != str2);
  cout << "result = " << result << endl; // => 1 (true)

  // concatenate two strings using overloaded +
  str3 = str1 + str2;
  cout << str3 << endl;

  // length of string
  unsigned long a = str3.length();
  cout << "length of string str3 = " << a << endl;           // => 51
  cout << "length of string str3 = " << str3.size() << endl; // => 51

  bool b = str1.empty();
  cout << "b = " << b << endl; // => 0 (false)

  string str4("Good Morning Egoli");
  string str5("XYZ1234");

  cout << "Initial string:\n";
  cout << "str4: " << str4 << endl;
  cout << "str5: " << str5 << endl;

  // method insert
  cout << "insert str5 into str4:\n";
  str4.insert(5,str5);
  cout << str4 << "\n"; // => Good XYZ1234Morning Egoli

  // method erase
  cout << "remove 7 characters from str4:\n";
  str4.erase(5,7);
  cout << str4 << "\n";  // => Good Morning Egoli

  // method replace
  cout << "replace 2 characters in str4 with str5:\n";
  str4.replace(5,2,str5);
  cout << str4 << endl;  // => Good XYZ1234rning Egoli

  // substring
  cout << "substring = " << str4.substr(2,5); // => od XY
 
  return 0;
}
