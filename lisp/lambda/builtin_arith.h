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


// builtin_arith.h

#ifndef BUILTIN_ARITH_H
#define BUILTIN_ARITH_H

#include "expression.h"

expression builtin_add(expression &e,environment &env)
{
 double acc = 0.0;
 vector<expression>::iterator j = e.tokens.begin() + 1;
 while(j != e.tokens.end()) acc += (j++)->numeric(env);
 return expression(acc);
}

expression builtin_sub(expression &e,environment &env)
{
 int first = 1;
 double acc = 0.0;
 vector<expression>::iterator j = e.tokens.begin() + 1;
 if(e.tokens.size()==2) first = 0;
 while(j!=e.tokens.end())
 {
  if(first) {acc = (j++)->numeric(env); first = 0;}
  else acc -= (j++)->numeric(env);
 }
 return expression(acc);
}

expression builtin_mul(expression &e,environment &env)
{
 double acc = 1.0;
 vector<expression>::iterator j = e.tokens.begin() + 1;
 while(j!=e.tokens.end()) acc *= (j++)->numeric(env);
 return expression(acc);
}

expression builtin_div(expression &e,environment &env)
{
 int first = 1;
 double acc = 1.0;
 vector<expression>::iterator j = e.tokens.begin() + 1;
 if(e.tokens.size()==2) first = 0;
 while(j!=e.tokens.end())
 {
  if(first) {acc = (j++)->numeric(env); first = 0;}
  else acc /= (j++)->numeric(env);
 }
 return expression(acc);
}

expression builtin_mod(expression &e,environment &env)
{
 int first = 1;
 double acc = 1.0;
 vector<expression>::iterator j = e.tokens.begin() + 1;
 while(j!=e.tokens.end())
 {
  if(first) {acc = (j++)->numeric(env); first = 0;}
  else acc = fmod(acc, (j++)->numeric(env));
 }
 return expression(acc);
}

expression builtin_equality(expression &e,environment &env)
{
 int holds = 1, first = 1;
 double i, i0;
 vector<expression>::iterator j = e.tokens.begin() + 1;
 while(j!=e.tokens.end())
 {
  i = (j++)->numeric(env);
  if(first) {i0 = i; first = 0;}
  else if(i!=i0) {holds = 0; break;};
 }
 return expression((holds)?"true":"false");
}

expression builtin_greater(expression &e,environment &env)
{
 int holds = 1, first = 1;
 double i, i0;
 vector<expression>::iterator j = e.tokens.begin() + 1;
 while(j!=e.tokens.end())
 {
  i = (j++)->numeric(env);
  if(first) {i0 = i; first = 0;}
  else if(i>=i0) {holds = 0; break;};
  i0 = i;
 }
 return expression((holds)?"true":"false");
}
#endif
