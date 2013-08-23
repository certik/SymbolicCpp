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


// teleport.cpp

#include <iostream>
#include "symbolicc++.h"
using namespace std;

Symbolic Hadamard(const Symbolic &v)
{
 Symbolic H("",2,2);
 Symbolic sqrt12 = sqrt(1/Symbolic(2));
 H(0,0) = sqrt12; H(0,1) =  sqrt12;
 H(1,0) = sqrt12; H(1,1) = -sqrt12;
 return (H*v);
}

Symbolic XOR(const Symbolic &v)
{
 Symbolic X("",4,4);
 X(0,0) = 1; X(0,1) = 0; X(0,2) = 0; X(0,3) = 0;
 X(1,0) = 0; X(1,1) = 1; X(1,2) = 0; X(1,3) = 0;
 X(2,0) = 0; X(2,1) = 0; X(2,2) = 0; X(2,3) = 1;
 X(3,0) = 0; X(3,1) = 0; X(3,2) = 1; X(3,3) = 0;
 return (X*v);
}

Symbolic Bell(const Symbolic &v)
{
 Symbolic I("",2,2), H("",2,2), X("",4,4);
 Symbolic sqrt12 = sqrt(1/Symbolic(2));

 I = I.identity();
 
 H(0,0) = sqrt12; H(0,1) =  sqrt12;
 H(1,0) = sqrt12; H(1,1) = -sqrt12;

 Symbolic UH = kron(H,I);

 X(0,0) = 1; X(0,1) = 0; X(0,2) = 0; X(0,3) = 0;
 X(1,0) = 0; X(1,1) = 1; X(1,2) = 0; X(1,3) = 0;
 X(2,0) = 0; X(2,1) = 0; X(2,2) = 0; X(2,3) = 1;
 X(3,0) = 0; X(3,1) = 0; X(3,2) = 1; X(3,3) = 0;

 return (X*(UH*v));
}

Symbolic Swap(const Symbolic &v)
{
 Symbolic S("",4,4);
 S(0,0) = 1; S(0,1) = 0; S(0,2) = 0; S(0,3) = 0;
 S(1,0) = 0; S(1,1) = 0; S(1,2) = 0; S(1,3) = 1;
 S(2,0) = 0; S(2,1) = 0; S(2,2) = 1; S(2,3) = 0;
 S(3,0) = 0; S(3,1) = 1; S(3,2) = 0; S(3,3) = 0;
 return XOR(S*XOR(v));
}

Symbolic Teleport(const Symbolic &v)
{
 Symbolic result;
 Symbolic NOT("",2,2),H("",2,2),I("",2,2),X("",4,4);
 Symbolic sqrt12 = sqrt(1/Symbolic(2));

 NOT(0,0) = 0; NOT(0,1) = 1;
 NOT(1,0) = 1; NOT(1,1) = 0;

 H(0,0) = sqrt12; H(0,1) =  sqrt12;
 H(1,0) = sqrt12; H(1,1) = -sqrt12;

 I = I.identity();

 X(0,0) = 1; X(0,1) = 0; X(0,2) = 0; X(0,3) = 0;
 X(1,0) = 0; X(1,1) = 1; X(1,2) = 0; X(1,3) = 0;
 X(2,0) = 0; X(2,1) = 0; X(2,2) = 0; X(2,3) = 1;
 X(3,0) = 0; X(3,1) = 0; X(3,2) = 1; X(3,3) = 0;

 Symbolic U1=kron(I,kron(H,I));
 Symbolic U2=kron(I,X);
 Symbolic U3=kron(X,I);
 Symbolic U4=kron(H,kron(I,I));
 Symbolic U5=kron(I,X);
 Symbolic U6=kron(I,kron(I,H));
 Symbolic U7=dsum(I,dsum(I,dsum(NOT,NOT)));
 Symbolic U8=kron(I,kron(I,H));

 result=U8*(U7*(U6*(U5*(U4*(U3*(U2*(U1*v)))))));

 return result;
}

// The outcome after measuring value for qubit.
// Since the probabilities may be symbolic this function
// cannot simulate a measurement where random outcomes
// have the correct distribution
Symbolic Measure(const Symbolic &v,unsigned int qubit,unsigned int value)
{
 int i,len,skip = 1-value;
 Symbolic result(v);
 Symbolic D;

 len = v.rows()/int(pow(2.0,qubit+1.0));
 for(i=0;i<v.rows();i++)
 {
  if(!(i%len)) skip = 1-skip;
  if(skip) result(i) = 0;
  else D += result(i)*result(i);
 }
 return result/sqrt(D);
}

// for output clarity
ostream &print(ostream &o, const Symbolic &v)
{
 char *b2[2]={"|0>","|1>"};
 char *b4[4]={"|00>","|01>","|10>","|11>"};
 char *b8[8]={"|000>","|001>","|010>","|011>",
              "|100>","|101>","|110>","|111>"};
 char **b; 
 int i;

 if(v.rows()==2) b=b2;
 if(v.rows()==4) b=b4;
 if(v.rows()==8) b=b8;

 for(i=0;i<v.rows();i++)
  if(v(i)!=0) o << "+(" << v(i) << ")" << b[i];

 return o;
}

int main(void)
{
 Symbolic zero("", 2),one("", 2);
 Symbolic zz("", 4),zo("", 4),oz("", 4),oo("", 4),qreg;
 Symbolic tp00,tp01,tp10,tp11,psiGHZ;
 Symbolic a("a"), b("b");
 Symbolic sqrt12 = sqrt(1/Symbolic(2));

 zero(0) = 1; zero(1) = 0;
 one(0)  = 0; one(1)  = 1;
 zz = kron(zero,zero);
 zo = kron(zero,one);
 oz = kron(one,zero);
 oo = kron(one,one);

 cout << "UH|0> = "; print(cout,Hadamard(zero))<< endl;
 cout << "UH|1> = "; print(cout,Hadamard(one)) << endl;
 cout << endl;
 cout << "UXOR|00> = "; print(cout,XOR(zz)) << endl;
 cout << "UXOR|01> = "; print(cout,XOR(zo)) << endl;
 cout << "UXOR|10> = "; print(cout,XOR(oz)) << endl;
 cout << "UXOR|11> = "; print(cout,XOR(oo)) << endl;
 cout << endl;
 cout << "UBELL|00> = "; print(cout,Bell(zz)) << endl;
 cout << "UBELL|01> = "; print(cout,Bell(zo)) << endl;
 cout << "UBELL|10> = "; print(cout,Bell(oz)) << endl;
 cout << "UBELL|11> = "; print(cout,Bell(oo)) << endl;
 cout << endl;
 cout << "USWAP|00> = "; print(cout,Swap(zz)) << endl;
 cout << "USWAP|01> = "; print(cout,Swap(zo)) << endl;
 cout << "USWAP|10> = "; print(cout,Swap(oz)) << endl;
 cout << "USWAP|11> = "; print(cout,Swap(oo)) << endl;
 cout << endl;

 qreg=kron(a*zero+b*one,kron(zero,zero));
 cout << "UTELEPORT("; print(cout,qreg) << ") = ";
  print(cout,qreg=Teleport(qreg)) << endl;
 cout << "Results after measurement of first 2 qubits:" << endl;
 tp00 = Measure(Measure(qreg,0,0),1,0);
 tp01 = Measure(Measure(qreg,0,0),1,1);
 tp10 = Measure(Measure(qreg,0,1),1,0);
 tp11 = Measure(Measure(qreg,0,1),1,1);

 Equations simplify = (a*a == 1 - b*b, 1/sqrt(1/Symbolic(4)) == 2);

 tp00 = tp00.subst_all(simplify);
 tp01 = tp01.subst_all(simplify);
 tp10 = tp10.subst_all(simplify);
 tp11 = tp11.subst_all(simplify);

 cout << " |00> : " ; print(cout,tp00) << endl;
 cout << " |01> : " ; print(cout,tp01) << endl;
 cout << " |10> : " ; print(cout,tp10) << endl;
 cout << " |11> : " ; print(cout,tp11) << endl;
 cout << endl;

 psiGHZ=kron(zz,zero)*sqrt12+kron(oo,one)*sqrt12;
 cout << "Greenberger-Horne-Zeilinger state : ";
 print(cout,psiGHZ) << endl;
 cout << "Measuring qubit 0 as 1 yields : ";
 print(cout,Measure(psiGHZ,0,1)) <<endl;
 cout << "Measuring qubit 1 as 1 yields : ";
 print(cout,Measure(psiGHZ,1,1)) <<endl;
 cout << "Measuring qubit 2 as 0 yields : ";
 print(cout,Measure(psiGHZ,2,0)) <<endl;

 return 0;
}
