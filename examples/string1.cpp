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


// string1.cpp

#include <iostream>
#include <cstring>  // for strlen, strcpy, strcat, strncpy
#include <cassert>  // for assert
using namespace std;

	      // Declaration of class String
class String
{
  private:
     char* s;
     int length;
  public:
    String(const char*);
    String(unsigned);
    String(unsigned,char);
    String(const String&);    // copy constructor
    ~String();                // destructor
    String& operator = (const String&);
    String operator () (unsigned,unsigned) const;
    String operator () (unsigned) const;
    char& operator [] (unsigned);
    operator const char* () const;
    friend String operator + (const String&,const String&);
    friend int operator == (const String&,const String&);
    friend int operator != (const String&,const String&);
    friend int operator < (const String&,const String&);
    friend int operator <= (const String&,String&);
    friend ostream& operator << (ostream&,const String&);
    friend istream& operator >> (istream&,String&);
    void display();
    void read(unsigned);
    String swap_char(unsigned,unsigned);
    int replace(String,String);
    String reverse() const;
    void fill(char);
};

String::String(const char* p)
{
  length = strlen(p);
  s = new char[length+1];
  strcpy(s,p);
}

String::String(unsigned len)
{
  length = len;
  s = new char[length+1];
  s[length] = '\0';
}

String::String(unsigned len,char tofill)
{
  length = len;
  s = new char[length+1];
  fill(tofill);
  s[length] = '\0';
}

String::String(const String& toCopy)
{
  length = toCopy.length;
  s = new char[length+1];
  strcpy(s,toCopy.s);
}

String::~String()
{ delete[] s; }

String& String::operator = (const String& toSet)
{
  if(this == &toSet) return *this;
  if(length != toSet.length)
  {
   delete[] s;
   length = toSet.length;
   s = new char[length+1];
  }
  strcpy(s,toSet.s);
  return *this;
}

String String::operator () (unsigned start,unsigned end) const
{
  assert(start > 0 && end <= length && start <= end);
  int newlength = end-start+1;
  String S(newlength);
  strncpy(S.s,s+start-1,newlength);
  return S;
}

String String::operator() (unsigned end) const
{
  assert(end > 0 && end <= length);
  return (*this)(1,end);
}

char& String::operator[] (unsigned i)
{
  assert(i > 0 && i <= length);
  return s[i-1];
}

String::operator const char* () const
{ return s; }

String operator + (const String& s1,const String& s2)
{
  String S(s1.length + s2.length);
  strcpy(S.s,s1.s);
  strcat(S.s,s2.s);
  return S;
}

int operator == (const String& s1,const String& s2)
{ return !strcmp(s1.s,s2.s); }

int operator != (const String& s1,const String& s2)
{ return !(s1 == s2); }

int operator < (const String& s1,const String& s2)
{ return strcmp(s1.s,s2.s) < 0; }

int operator <= (const String& s1,const String& s2)
{ return (s1 < s2 || s1 == s2); }

ostream& operator << (ostream& out,const String& S)
{
  out << S.s;
  return out;
}

istream& operator >> (istream& in,String& S)
{
  const int max = 1024;
  delete[] S.s;
  S.s = new char[max];
  in.width(max);
  in >> S.s;
  S.length = strlen(S.s);
  return in;
}

void String::display()
{ cout << s; }

void String::read(unsigned i)
{
  length = i;
  delete[] s;
  s = new char[length+1];
  cin.width(i+1);
  cin >> s;
}

String String::swap_char(unsigned int n,unsigned int m)
{
  String S(length);
  char temp;
  strcpy(S.s,s);
  temp = S[n]; S[n] = S[m]; S[m] = temp;
  return S;
}

int String::replace(String sub,String new_sub)
{
  char *temp, *ptr, *base;
  int len, diff;
  if(length == 0) return 0;
  else
  {
    diff = new_sub.length-sub.length;
    if(diff > 0) // string must grow
    {
     // after substitution, at most length/sub.length
     // substrings will have been replaced causing the 
     // string length to grow by length*diff/sub.length
     temp = new char[length+length*diff/sub.length];
     strcpy(temp,s);
     delete[] s;
     s = temp;
    }
    temp = new char[length];
    len = sub.length;
    base = ptr = s;
    while((base = strstr(base,sub.s)) != NULL) 
    {
     ptr = base+len;
     strncpy(temp,ptr,strlen(ptr)+1);
     strcpy(base,new_sub.s);
     strcpy(base + new_sub.length,temp);
     // the string length changed after substitution
     length += diff;
     // the substituted string is not subject to substitution again
     base = base + new_sub.length;
    }
    if(ptr == s)
     cout << "sorry substring cannot be replaced.\n";
    else
     cout << "The new string is " << s << endl;
    delete[] temp;
    return 1;
  }
}

String String::reverse() const
{
  String S(length);
  for(int i=0;i<length;i++) S.s[i] = s[length-i-1];
  return S;
}

void String::fill(char tofill)
{ for(int i=0;i<length;s[i++]=tofill); }

int main(void)
{
  String S1("Johannesburg");
  cout << "S1 = " << S1 << endl;  // Johannesburg
  S1.display();                   // Johannesburg
  cout << endl;

  String S2(3);
  cout << "S2 = " << S2 << endl;

  String S3(6,'$');
  cout << "S3 = " << S3 << endl;  // $$$$$$

  String S4(S1);
  cout << "S4 = " << S4 << endl;

  S2 = S4;
  cout << "S2 = " << S2 << endl;  // Johannesburg

  S4.fill('+');
  cout << "S4 = " << S4 << endl;  //  ++++++++++++
  cout << "S2 = " << S2 << endl;  //  Johannesburg

  cout << "reverse of S1: " << S1.reverse() << endl; // grubsennahoJ
  S1.reverse().display();                            // grubsennahoJ
  cout << endl;

  String S5(30);
  S5 = S1 + S1.reverse();
  cout << "S5 = " << S5 << endl;    // JohannesburggrubsennahoJ

  cout << "S1 == S2 --> " << (S1 == S2) << endl;   // 1
  cout << "S1 == S4 --> " << (S1 == S4) << endl;   // 0

  cout << "S1.swap_char(2,6): " << S1.swap_char(2,6); // Jnhanoesburg
  cout << endl;

  String S6(S1);
  cout << "S6 = " << S6 << endl;
  S6.replace("hannes", "");
  cout << "S6.replace(\"hannes\", \"\") --> " << S6 << endl; //Joburg

  String S7(S1);
  cout << "S7 = " << S7 << endl;
  S7.replace("n", "nn");
  cout << "S7.replace(\"n\", \"nn\") --> " << S7 << endl; //Johannnnesburg

  return 0;
}
