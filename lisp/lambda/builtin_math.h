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


// builtin_math.h

#ifndef BUILTIN_MATH_H
#define BUILTIN_MATH_H

expression builtin_sin(expression &e,environment &env)
{
 if(e.tokens.size()!=2)
 {
  cerr << "Error: incorrect number of arguments to sin\n";
  return expression("#error");
 }
 return expression(sin(e.tokens[1].numeric(env)));
}

expression builtin_cos(expression &e,environment &env)
{
 if(e.tokens.size()!=2)
 {
  cerr << "Error: incorrect number of arguments to cos\n";
  return expression("#error");
 }
 return expression(cos(e.tokens[1].numeric(env)));
} 
  
expression builtin_exp(expression &e,environment &env)
{
 if(e.tokens.size()!=2)
 {
  cerr << "Error: incorrect number of arguments to exp\n";
  return expression("#error");
 }
 return expression(exp(e.tokens[1].numeric(env)));
} 
  
expression builtin_log(expression &e,environment &env)
{
 if(e.tokens.size()!=2)
 {
  cerr << "Error: incorrect number of arguments to log\n";
  return expression("#error");
 }
 return expression(log(e.tokens[1].numeric(env)));
} 
  
expression builtin_sqrt(expression &e,environment &env)
{
 if(e.tokens.size()!=2)
 {
  cerr << "Error: incorrect number of arguments to sqrt\n";
  return expression("#error");
 }
 return expression(sqrt(e.tokens[1].numeric(env)));
} 
#endif
