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


// fermi.cpp

#include <iostream>
#include "symbolicc++.h"
using namespace std;

const int n=4;

enum { create, annihilate };
enum { up, down };
enum { x, y, z };

Symbolic vacuum = ~Symbolic("|0>");
Symbolic vacuum_dual = ~Symbolic("<0|");
// ~ => non-commutative
Symbolic c[2] = { ~Symbolic("cd", n, 2),
                  ~Symbolic("c",  n, 2) };

// i = sqrt(-1)
using SymbolicConstant::i;

Equations fermi_relations(void)
{
 int i, j, k, l, s, t;

 Equations relations;
 
 // <0|0> = 1 and 1/i = -i
 relations = (relations, vacuum_dual * vacuum == 1,
              1/SymbolicConstant::i == -SymbolicConstant::i,
              ((2*SymbolicConstant::i)^(-1)) == -SymbolicConstant::i/2);

 // c |0>  = 0
 // <0| cd = 0
 for(i=0;i<n;i++)
  for(j=up;j<=down;j++)
   relations = (relations,
                c[annihilate](i,j) * vacuum == 0,
                vacuum_dual * c[create](i,j) == 0);

 // cd c = I - c cd
 for(i=create;i<=annihilate;i++)
   for(j=create;j<=annihilate;j++)
    for(k=up;k<=down;k++)
     for(l=up;l<=down;l++)
      for(s=0;s<n;s++)
       for(t=0;t<n;t++)
       {
        if(i == j && k == l && s == t)
          relations = (relations, c[i](s,k) * c[i](s,k) == 0);
        else
        if(i == annihilate && j == create && k == l && s == t)
          relations = (relations, 
                       c[i](s,k) * c[j](s,k) == 1 - c[j](s,k) * c[i](s,k));
        else
        if((i == annihilate && j == create)           ||
           (i == j && t < s) ||
           (i == j && s == t && l == up && k == down))
          relations = (relations,
                       c[i](s,k) * c[j](t,l) == -c[j](t,l) * c[i](s,k));
       }
       
 return relations;
}

int main(void)
{
 Symbolic t("t"), U("U"), alpha("alpha"),
          ni("ni", n, 2), S("S", n, 3), R("R", n, 3);
 Equations relations = fermi_relations();

 // setup the spin and quasi-spin operators
 for(int j=0;j<n;j++)
 {
  for(int k=up;k<=down;k++)
   ni(j,k) = c[create](j,k)*c[annihilate](j,k);

  S(j,x) = (c[create](j,up)*c[annihilate](j,down)
           +c[create](j,down)*c[annihilate](j,up))/2;
  S(j,y) = (c[create](j,up)*c[annihilate](j,down)
           -c[create](j,down)*c[annihilate](j,up))/(2*i);
  S(j,z) = (ni(j,up) - ni(j,down))/2;

  S(j,x) = S(j,x).subst_all(relations);
  S(j,y) = S(j,y).subst_all(relations);
  S(j,z) = S(j,z).subst_all(relations);

  R(j,x) = (c[create](j,up)*c[create](j,down)
           +c[annihilate](j,down)*c[annihilate](j,up))/2;
  R(j,y) = (c[create](j,up)*c[create](j,down)
           -c[annihilate](j,down)*c[annihilate](j,up))/(2*i);
  R(j,z) = (ni(j,up) + ni(j, down) - 1)/2;

  R(j,x) = R(j,x).subst_all(relations);
  R(j,y) = R(j,y).subst_all(relations);
  R(j,z) = R(j,z).subst_all(relations);
 }

 Symbolic Ne, Sz, H, C;

 // setup the operators used for the commutation relations
 for(int j=0;j<n;j++)
 {
  Sz += S(j,z);
  H += U*ni(j,up)*ni(j,down);
  C += (c[create](j,up)*c[annihilate]((j+n-1)%n,up)
       -c[create]((j+n-1)%n,up)*c[annihilate](j,up))
       *(ni(j,down) + ni((j+n-1)%n,down))
     + (c[create](j,down)*c[annihilate]((j+n-1)%n,down)
       -c[create]((j+n-1)%n,down)*c[annihilate](j,down))
       *(ni(j,up) + ni((j+n-1)%n,up));

  for(int k=up;k<=down;k++)
  {
   Ne += ni(j,k);
   H += t*(c[create]((j+1)%n,k)*c[annihilate](j,k)
          +c[create](j,k)*c[annihilate]((j+1)%n,k));
   C -= c[create](j,k)*c[annihilate]((j+n-1)%n,k)
       -c[create]((j+n-1)%n,k)*c[annihilate](j,k);
  }
 }

 Ne = Ne.subst_all(relations);
 H  = H.subst_all(relations);
 C  = C.subst_all(relations);

 // verify identity (7)
 for(int i=0;i<4;i++)
 {
  Symbolic lefteq = ni(i,up)*ni(i,down);
  Symbolic righteq = (1-alpha)/4+R(i,z)
                   + (alpha-1)*(S.row(i) | S.row(i)) / 3
                   + (alpha+1)*(R.row(i) | R.row(i)) / 3;
  lefteq = lefteq.subst_all(relations);
  righteq = righteq.subst_all(relations);
  if(lefteq == righteq)
   cout << "Identity (7) holds for i = " << i << endl;
  else
   cout << "Identity (7) does not hold for i = " << i << endl
        << (lefteq == righteq) << endl;
 }

 // verify the commutation relations for the spin
 // and quasi-spin operators
 cout<<"The commutation relations are"<<endl;
 for(int i=0;i<4;i++)
  for(int j=x;j<=z;j++)
  for(int k=j+1;k<=z;k++)
  {
   Symbolic commutator;

   commutator = (S(i,j)*S(i,k)-S(i,k)*S(i,j)).subst_all(relations);
   cout << "[S(" << i << "," << j << "),S(" << i << "," << k <<")] = "
        << commutator;
   if(commutator == SymbolicConstant::i*S(i,z))
    cout << " == i*S(" << i << ",z)";
   if(commutator == -SymbolicConstant::i*S(i,y))
    cout << " == -i*S(" << i << ",y)";
   if(commutator == SymbolicConstant::i*S(i,x))
    cout << " == i*S(" << i << ",x)";
   cout << endl;

   commutator = (R(i,j)*R(i,k)-R(i,k)*R(i,j)).subst_all(relations);
   cout << "[R(" << i << "," << j << "),R(" << i << "," << k <<")] = "
        << commutator;
   if(commutator == SymbolicConstant::i*R(i,z))
    cout << " == i*R(" << i << ",z)";
   if(commutator == -SymbolicConstant::i*R(i,y))
    cout << " == -i*R(" << i << ",y)";
   if(commutator == SymbolicConstant::i*R(i,x))
    cout << " == i*R(" << i << ",x)";
   cout << endl;
  }


 // determine the commutation relations

 Symbolic result = (Ne*H - H*Ne).subst_all(relations);
 cout << "[Ne,H]=" << result << endl;

 result = (Sz*H - H*Sz).subst_all(relations);
 cout << "[Sz,H]=" << result << endl;

 result = (C*H - H*C).subst_all(relations);
 cout << "[C,H]=" << result << endl;


 // find the matrix representation

 cout<<"Matrix representation of H"<<endl;
 for(int ib=0;ib<n;ib++)
  for(int jb=ib+1;jb<n;jb++)
   for(int mb=0;mb<n;mb++)
    for(int nb=mb+1;nb<n;nb++,cout<<endl)
 for(int ii=0;ii<n;ii++)
  for(int ji=ii+1;ji<n;ji++)
   for(int mi=0;mi<n;mi++)
    for(int ni=mi+1;ni<n;ni++)
    {
     Symbolic HM =
      vacuum_dual*c[annihilate](ni,down)*c[annihilate](mi,down)
                 *c[annihilate](ji,up)*c[annihilate](ii,up)
               *H*c[create](ib,up)*c[create](jb,up)
                 *c[create](mb,down)*c[create](nb,down)*vacuum;
     HM = HM.subst_all(relations);
     cout << HM << "\t"; cout.flush();
    }
 cout<<endl;

 return 0;
}
