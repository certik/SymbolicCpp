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


// spins3.cpp

#include <iostream>
#include <fstream>
#include "symbolicc++.h"
using namespace std;

ofstream fout("spinS3.dat");

Symbolic sigma(char coord,int index,int N)
{
  int i;
  Symbolic I("I",2,2);
  I = I.identity();
  Symbolic result, s("s",2,2);

  if(coord=='x')
  { s(0,0) = 0; s(0,1) = 1; s(1,0) = 1; s(1,1) = 0; }
  else // 'z'
  { s(0,0) = 1; s(0,1) = 0; s(1,0) = 0; s(1,1) = -1; }

  if(index==0)
  {
   result = s;
   for(i=1;i<N;i++) result = kron(result,I);
  }
  else
  {
   result = I;
   for(i=1;i<index;i++)  result = kron(result,I);
   result = kron(result,s);
   for(i=index+1;i<N;i++) result = kron(result,I);
  }
  return result;
}

Symbolic H(int N,Symbolic a,Symbolic b)
{
  int i;
  Symbolic result, part, sigmaX("X",N), sigmaZ("Z",N);

  for(i=0;i<N;i++) sigmaX(i) = sigma('x',i,N);
  for(i=0;i<N;i++) sigmaZ(i) = sigma('z',i,N);

  part = 0;
  for(i=1;i<=N;i++) part += sigmaZ(i-1)*sigmaZ(i%N);
  result = a*part;

  part = 0;
  for(i=0;i<N;i++) part += sigmaX(i);
  result += b*part;

  return result;
}

int main(void)
{
  int N=3;
  Symbolic c = Symbolic(1)/2, d = Symbolic(1)/3;
  Symbolic a("a"), b("b"), p("p"), det;
  Symbolic result;

  result = H(N,a,b);
  fout << result << endl;
  fout << "trace = " << result.trace() << endl;

  det = result.determinant();
  fout << "determinant = " << det << endl;
  fout << endl;

  // assigning numerical values
  fout << "Put a = " << c << " and b = " << d << endl;
  fout << endl;

  fout << "The matrix becomes:" << endl;
  fout << result[a == c, b == d] << endl;
  fout << "determinant = " << det[a == c, b == d] << endl;
  return 0;
}
