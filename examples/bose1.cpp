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


// bose1.cpp

#include <iostream>
using namespace std;

// Declaration of class State
class State
{
  private:
    int m;
    int factor;
  public:
    State();                                 // constructor
    State(const State&);                     // copy constructor
    const State& operator = (const State&);  // overloading =
    void bose_state(int);                    // member function
    void display();                          // member function
};

// Implementation of class State
State::State()
{
  m = 0;
  factor = 1;
}

State::State(const State& arg)
{
  m = arg.m;
  factor = arg.factor;
}

const State& State::operator = (const State& arg)
{
  cout << "overloaded = invoked" << endl;
  m = arg.m;
  factor = arg.factor;
  return *this;
}

void State::bose_state(int cd)
{
  if(cd == -1) factor *= m--;
  else m++;
}

void State::display()
{
  if(factor == 0) cout << "0";
  else
  {
   cout << factor << "*";
   if(m != 0)
   {
    cout << "(";
    for(int i=0;i<m;i++) cout << "b+";
    cout << ")";
   }
   cout << "|0>";
  }
}

// Declaration of class Bose
class Bose
{
  public:
    Bose();                           // constructor
    State operator + (const State&);  // overloading +
    State operator - (const State&);  // overloading -
};

// Implementation of class Bose
Bose::Bose() { }  // default constructor

State Bose::operator + (const State& st2)
{
  State st1 = st2;
  st1.bose_state(1);
  return st1;
}

State Bose::operator - (const State& st2)
{
  State st1 = st2;
  st1.bose_state(-1);
  return st1;
}

int main(void)
{
  State g1;
  Bose b;
  g1 = b- (b- (b+ (b+ (b+ (b+ (b+ (b+ g1)))))));
  cout << "g1 = ";
  g1.display(); // output 30*(b+b+b+b+)|0>
  cout << endl;
  State g2;
  g2 = b+ (b- g2);
  g2.display();        // output 0
  return 0;
}
