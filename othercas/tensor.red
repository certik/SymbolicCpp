

%%    SymbolicC++ : An object oriented computer algebra system written in C++
%%
%%    Copyright (C) 2008 Yorick Hardy and Willi-Hans Steeb
%%
%%    This program is free software; you can redistribute it and/or modify
%%    it under the terms of the GNU General Public License as published by
%%    the Free Software Foundation; either version 2 of the License, or
%%    (at your option) any later version.
%%
%%    This program is distributed in the hope that it will be useful,
%%    but WITHOUT ANY WARRANTY; without even the implied warranty of
%%    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%    GNU General Public License for more details.
%%
%%    You should have received a copy of the GNU General Public License along
%%    with this program; if not, write to the Free Software Foundation, Inc.,
%%    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.


% tensor.red

matrix g(2,2);
matrix g1(2,2);  % inverse of g;
array gamma(2,2,2); array R(2,2,2,2); array Ricci(2,2);

operator u, x;

g(1,1) := 1;  g(2,2) := 1;
g(1,2) := cos(u(x(1),x(2))); g(2,1) := cos(u(x(1),x(2)));

g1 := g^(-1);    % calculating the inverse
for a := 1:2 do
   for m := 1:2 do
      for n := 1:2 do
      gamma(a,m,n) := (1/2)*
                      (for b := 1:2 sum g1(a,b)*(df(g(b,m),x(n))
                           + df(g(b,n),x(m)) - df(g(m,n),x(b))));

for a := 1:2 do
   for m := 1:2 do
      for n := 1:2 do
      write "gamma(",a,",",m,",",n,") = ", gamma(a,m,n);

for b := 1:2 do
   for m := 1:2 do
      for s := 1:2 do
         for q := 1:2 do
         R(b,m,s,q) := df(gamma(b,m,q),x(s))-df(gamma(b,m,s),x(q))
                       + (for n := 1:2 sum gamma(b,n,s)*gamma(n,m,q))
                       - (for n := 1:2 sum gamma(b,n,q)*gamma(n,m,s));

cos(u(x(1),x(2)))**2 := 1 - sin(u(x(1),x(2)))**2;

for m := 1:2 do
   for q := 1:2 do
   Ricci(m,q) := for s := 1:2 sum R(s,m,s,q);

for m := 1:2 do
   for q := 1:2 do
   write "Ricci(",m,",",q,") = ", Ricci(m,q);

array Ricci1(2,2);
for m := 1:2 do
   for q := 1:2 do
   Ricci1(m,q) := (for b := 1:2 sum g1(m,b)*Ricci(q,b));

CS := for m := 1:2 sum Ricci1(m,m);
