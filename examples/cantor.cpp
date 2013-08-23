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


// cantor.cpp

#include <iostream>
#include "rational.h"
#include "verylong.h"
#include "vector.h"
using namespace std;

const Rational<Verylong> a = Rational<Verylong>("0"); // lower limit
const Rational<Verylong> b = Rational<Verylong>("1"); // upper limit

class Cantor
{
  private:
    Vector<Rational<Verylong> > CS;
    int currentSize;
  public:
    Cantor(int);
    Cantor(const Cantor&);  // copy constructor
    int step();
    void run();
    friend ostream& operator << (ostream&,const Cantor&);
};

Cantor::Cantor(int numStep) : CS((int)pow(2.0,numStep+1)), currentSize(2)
   {  CS[0] = a; CS[1] = b; }

Cantor::Cantor(const Cantor& s) 
   : CS(s.CS), currentSize(s.currentSize) { }

int Cantor::step()
{
  int i, newSize;
  static Rational<Verylong> three(3), tt(2,3);
  static int maxSize = CS.size();

  if(currentSize < maxSize)
  {
  for(i=0;i<currentSize;i++) CS[i] /= three;
  newSize = currentSize + currentSize;
  for(i=currentSize;i<newSize;i++) CS[i] = CS[i-currentSize] + tt;
  currentSize = newSize;
  return 1;
  }
  return 0;
}

void Cantor::run() {  while(step() != 0); }

ostream& operator << (ostream& s,const Cantor& c)
{
  for(int i=0;i<c.currentSize;i+=2)
  {
  s << "[" << c.CS[i] << " ";
  s << c.CS[i+1] << "] ";
  }
  return s;
}

int main(void)
{
  const int N = 6;
  Cantor C(N);
  cout << C << endl;
  for(int i=0;i<N;i++) { C.step(); cout << C << endl; }
  return 0;
}

