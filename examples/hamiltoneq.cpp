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


// hamiltoneq.cpp

#include <iostream>
#include "symbolicc++.h"
using namespace std;

int main(void)
{
  Symbolic h("h"), q1("q1"), q2("q2"), p1("p1"), p2("p2"), pt1, pt2, qt1, qt2;

  // Hamilton function
  h = (p1*p1+p2*p2+q1*q1+q2*q2)/2+q1*q1*q2-q2*q2*q2/3; 
  // Hamilton equations of motion
  pt1 = -df(h,q1); pt2 = -df(h,q2);
  qt1 = df(h,p1);  qt2 = df(h,p2);

  cout << "dp1/dt = " << pt1 <<endl;
  cout << "dp2/dt = " << pt2 <<endl; 
  cout << "dq1/dt = " << qt1 <<endl; 
  cout << "dq2/dt = " << qt2 <<endl;

  return 0;
}

