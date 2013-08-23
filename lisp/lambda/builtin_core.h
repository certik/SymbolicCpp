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


// builtin_core.h

#ifndef BUILTIN_CORE_H
#define BUILTIN_CORE_H

#include "expression.h"

expression builtin_apply(expression &e,environment &env)
{ 
 expression e1 = e;
 if(e1.tokens.size()==3)
 {
  e1.tokens[1] = evaluate(e1.tokens[1], env);
  e1.tokens[2] = evaluate(e1.tokens[2], env);
 }
 if(e1.tokens.size()!=3 || e1.tokens[2].type!=LIST)
 {
  cerr << "Error: apply takes two arguments,"
       << " a function and a list\n";
  return expression("#error");
 }
 e1.tokens[2].tokens.insert(e1.tokens[2].tokens.begin(),
                            1, e1.tokens[1]);
 e1.tokens[2].type = APPLICATION;
 return e1.tokens[2];
}

expression builtin_define(expression &e,environment &env)
{
 if(e.tokens[1].type!=FREE_VARIABLE)
 {
  cerr << "Error: define must bind a free variable.\n";
  return expression("#error");
 }
 define(e.tokens[1].identifier,e.tokens[2],env);
 return expression("#definition:" + e.tokens[1].identifier);
}

expression builtin_definedp(expression &e,environment &env)
{
 if(e.tokens[1].type!=FREE_VARIABLE)
 {
  cerr << "Error: defined? takes a free variable as argument\n";
  return expression("#error");
 }
 try
 {
  lookup(e.tokens[1].identifier,env);
  return expression("true");
 }
 catch(string error) {return expression("false");}
 return expression("#error");
}

expression builtin_sequence(expression &e,environment &env)
{ 
 expression e1;
 vector<expression>::iterator j = e.tokens.begin() + 1;
 while(j!=e.tokens.end()) e1 = evaluate(*(j++),env);
 return e1;
}

expression builtin_sequence_star(expression &e,environment &env)
{ 
 expression e1;
 environment local = env;
 vector<expression>::iterator j = e.tokens.begin() + 1;
 while(j!=e.tokens.end()) e1 = evaluate(*(j++),local);
 return e1;
}
#endif
