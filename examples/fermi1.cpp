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


// fermi1.cpp

#include <iostream>
#include <cassert>
#include <string>
using namespace std;

class Power
{
  public:
    string c; // the operator name, e.g. c1, c4+
    int n;    // the degree of the operator, e.g. (c4+)^2
};

class State
{
  private:
    int factor; // the multiplication of the state
    int m;      // number of distinctive operators
    Power *p;   // a pointer to Power

  public:
    State();                        // default constructor
    void operator = (const State&); // assignment operator
    void Fermi_creator(string,int); // operation on the state
    void display() const;           // display the state
    void reset();                   // reset the state
};

   // Fermi operators
class Fermi
{
  private:
    string f;  // store the name of the operator, e.g. c1, c4

  public:
     Fermi(string);                    // constructor
     State operator + (const State&);  // creation operator
     State operator - (const State&);  // annihilation operator
};

State::State() : factor(1), m(0), p(NULL) {}

void State::operator = (const State& s2)
{
  m = s2.m;
  p = new Power[m]; assert(p);
  for(int i=0;i<m;i++) p[i] = s2.p[i];
  factor = s2.factor;
}

void State::Fermi_creator(string ch,int s)
{
  int i;
  if(factor)
  {
   // [c1,c2]_+ = [c1+,c2+]_+ = [c1,c2+]_+ = 0
   for(i=0;i<m && (ch != (p+i)->c);i++)
    if((p+i)->n % 2) factor *= (-1);
   // if there is a new operator
   if(i==m)
   {
     if(s==1) // creation operator
     {
      Power *p2;
      m++;
      p2 = new Power[m]; assert(p2);
      for(int j=0;j<m-1;j++) p2[j] = p[j];
      (p2+m-1)->c = ch; (p2+m-1)->n = 1;
      delete[] p;
      p = p2;
     }
     else  // annihilation operator
     { m=0; factor=0; delete[] p;}
   }
   else // operator appears before
   {
    // creation operator, c1+ (c1+)^n = (c1+)^(n+1)
    if(s==1) (p+i)->n++;
    else // annihilation operator, [c1,c2+]_+ = I
    {
     if((p+i)->n%2) // if power of operator is odd
     {
      (p+i)->n--;
      if(!(p+i)->n)
      {
       for(int j=i+1;j<m;j++) p[j-1] = p[j];
       m--;
      }
     }
     else // if power of operator is even
     {  m=0; factor=0; delete[] p; }
    }
   }
  }
}

void State::display() const
{
  if(!factor) cout << "0";
  else
  {
   if(factor != 1) cout << "(" << factor << ")*";
   for(int i=0;i<m;i++)
   {
    cout << " " << (p+i)->c << "+";
    if((p+i)->n != 1) cout << "^" << (p+i)->n;
   }
   cout << "|0>";
  }
} // end display()

void State::reset()
{
  m = 0; factor = 1; 
  delete[] p; p = NULL;
}

Fermi::Fermi(string st) : f(st) { }

State Fermi::operator + (const State &s2)
{
  State s1(s2);
  s1.Fermi_creator(f,1);
  return s1;
}

State Fermi::operator - (const State &s2)
{
  State s1(s2);
  s1.Fermi_creator(f,-1);
  return s1;
}

int main(void)
{
  State g;
  Fermi c1("c1"), c4("c4");
  g = c1-  g;
  cout << "g = "; g.display(); cout << endl;
  g.reset();
  g = c1+  g;
  cout << "g = "; g.display(); cout << endl;
  g.reset();
  g = c1- (c1+ g);
  cout << "g = "; g.display(); cout << endl;
  g.reset();
  g = c4- (c4+ (c1+ (c4+ g)));
  cout << "g = "; g.display(); cout << endl;
  g.reset();
  g = (c4- (c4+ (c4+ (c1+ (c4+ g)))));
  cout << "g = "; g.display(); cout << endl;
  g.reset();
  g = c4- (c4+ (c4- (c4+ (c4- (c4+ g)))));
  cout << "g = "; g.display(); cout << endl;
  return 0;
}
