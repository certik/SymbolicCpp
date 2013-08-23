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


// builtin_list.h

#ifndef BUILTIN_LIST_H
#define BUILTIN_LIST_H

#include "expression.h"

expression builtin_car(expression &e,environment &env)
{
 expression e1 = e;
 if(e1.tokens.size()==2) e1.tokens[1] = evaluate(e1.tokens[1],env);
 if(e1.tokens.size()!=2 || e1.tokens[1].type!=LIST
                        || e1.tokens[1].tokens.size()<1)
 {
  cerr << "Error: car only operates on lists"
       << " with at least one element.\n";
  return expression("#error");
 }
 return e1.tokens[1].tokens[0];
}

expression builtin_cdr(expression &e,environment &env)
{
 expression e1 = e;
 if(e1.tokens.size()==2) e1.tokens[1] = evaluate(e1.tokens[1],env);
 if(e1.tokens.size()!=2 || e1.tokens[1].type!=LIST
                        || e1.tokens[1].tokens.size()<1)
 {
  cerr << "Error: cdr only operates on lists"
       << " with at least one element.\n";
  return expression("#error");
 }
 expression e2 = e1.tokens[1];
 e2.tokens.erase(e2.tokens.begin());
 return e2;
}

expression builtin_cons(expression &e,environment &env)
{
 expression e1 = e;
 if(e1.tokens.size()==3)
 {
  e1.tokens[1] = evaluate(e1.tokens[1],env);
  e1.tokens[2] = evaluate(e1.tokens[2],env);
 }
 if(e1.tokens.size()!=3 || e1.tokens[2].type!=LIST)
 {
  cerr << "Error: cons takes two arguments, the second a list.\n";
  return expression("#error");
 }
 e1.tokens[2].tokens.insert(e1.tokens[2].tokens.begin(),1,e1.tokens[1]);
 return e1.tokens[2];
}

expression builtin_emptyp(expression &e,environment &env)
{
 expression e1 = e;
 if(e1.tokens.size()==2) e1.tokens[1] = evaluate(e1.tokens[1],env);
 if(e1.tokens.size()!=2 || e1.tokens[1].type!=LIST)
 {
  cerr << "Error: empty? takes one argument of type list.\n";
  return expression("#error");
 }
 return expression((e1.tokens[1].tokens.size()==0)?"true":"false");
}

expression builtin_list(expression &e,environment &env)
{
 expression e1 = e;
 e1.tokens.erase(e1.tokens.begin());
 e1.type = LIST;
 return e1;
}
#endif
