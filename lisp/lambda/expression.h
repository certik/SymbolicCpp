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


// expression.h

#ifndef EXPRESSION_H
#define EXPRESSION_H

#include <iostream>
#include <fstream>
#include <map>
#include <sstream>
#include <string>
#include <vector>
using namespace std;

// lambda terms, with extra support for lists and numbers
typedef enum {
 APPLICATION, BOUND_VARIABLE, FREE_VARIABLE, LAMBDA, LIST, NUMBER
} expression_type;

class expression;

typedef map<string,expression> environment;
void define(string&,expression&,environment&);
expression lookup(string&,environment&);
expression evaluate(expression,environment&);
void interpreter(istream&,environment&);

typedef expression (*builtin)(expression&,environment&);
map<string,builtin> builtin_commands;

class expression
{
 public:
  expression_type type;
  string identifier;
  double number;
  vector<expression> tokens;
  expression *reference;

  void bind(expression &variable,expression &body);
  expression &copy(const expression &src,expression &dest);
  double numeric(environment &);

  expression() : type(FREE_VARIABLE), identifier("?") {}
  expression(string s) : type(FREE_VARIABLE), identifier(s) {}
  expression(double d) : type(NUMBER), number(d) {}
  expression(const expression &e) { copy(e, *this); }
  expression(vector<expression> &v);

  expression &operator=(const expression &e)
  { return copy(e,*this); }

  friend ostream &operator<<(ostream &,const expression&);
  friend istream &operator>>(istream &,expression&);
};

expression::expression(vector<expression> &v) : tokens(v)
{
 vector<expression> sublambda(3), lambda_variables;

 if(v.size()==3 && v[0].type==FREE_VARIABLE && v[0].identifier=="lambda")
 {
  type = LAMBDA;
  switch(v[1].type)
  {
   case FREE_VARIABLE:
    cerr << "Error: free variable as lambda variable\n"; break;
   case BOUND_VARIABLE:
    cerr << "Error: bound variable as lambda variable\n"; break;
   case LAMBDA:
    cerr << "Error: lambda expression as lambda variable\n"; break;
   case APPLICATION:
     tokens[1].type = LIST;
     switch(tokens[1].tokens.size())
     {
      case 0:  break;
      case 1:  bind(tokens[1].tokens[0],tokens[2]);
               break;
      default: lambda_variables
                 = vector<expression>(tokens[1].tokens.begin()+1,
                                      tokens[1].tokens.end()); 
               sublambda[0] = expression("lambda");
               sublambda[1] = expression(lambda_variables);
               sublambda[2] = expression(tokens[2]);
               tokens[1].tokens.erase(tokens[1].tokens.begin()+1,
                                      tokens[1].tokens.end());
               tokens[2] = expression(sublambda);
               bind(tokens[1].tokens[0],tokens[2]);
      }
   default:             break;
  }
 }
 else type = APPLICATION;
}

void expression::bind(expression &variable,expression &body)
{
 vector<expression>::iterator i;
 switch(body.type)
 {
  case FREE_VARIABLE: if(body.identifier==variable.identifier)
                      {
                       body.type = BOUND_VARIABLE;
                       body.reference = &variable;
                      }
                      break;
  case LAMBDA:        bind(variable,body.tokens[2]);
                      break;
  case APPLICATION:   for(i=body.tokens.begin();
                          i!=body.tokens.end();i++)
                       bind(variable,*i);
  default:            break;
 }
}

expression &expression::copy(const expression &src,expression &dest)
{
 if(&dest==&src) return dest;
 dest.type = src.type;
 expression *oldreference;
 static map<expression*, expression*> rebind;
 map<expression*, expression*>::iterator newbinding;
 
 switch(dest.type)
 {
  case FREE_VARIABLE:  dest.identifier = src.identifier; break;
  case NUMBER:         dest.number = src.number; break;
  // the next case is essentially alpha conversion
  // so that each abstraction has its own variable
  case BOUND_VARIABLE: dest.identifier = src.identifier;
                       newbinding = rebind.find(src.reference);
                       if(newbinding != rebind.end())
                        dest.reference = newbinding->second;
                       else copy(*(src.reference),dest);
                       break;
  // creates a new abstraction with its own variable
  // and applies alpha conversion
  case LAMBDA:         dest.tokens.resize(3);
                       copy(src.tokens[0],dest.tokens[0]);
                       copy(src.tokens[1],dest.tokens[1]);
                       oldreference
                         = (expression*)&(src.tokens[1].tokens[0]);
                       rebind[oldreference]
                         = &(dest.tokens[1].tokens[0]);
                       copy(src.tokens[2],dest.tokens[2]);
                       rebind.erase(oldreference);
                       break;
  case LIST:
  case APPLICATION:    dest.tokens = src.tokens; break;
  default:             break;
 }

 return dest;
}

double expression::numeric(environment &env)
{
 expression e = evaluate(*this,env);
 if(e.type==NUMBER) return e.number;
 if(e.type==FREE_VARIABLE)
 {
  double d;
  istringstream is(e.identifier);
  is >> d;
  return d;
 }
 cerr << "Error: Tried to use " << e << " as a number, using 0.0.\n";
 return 0.0;
}

void define(string &name,expression &definition,environment &env)
{env[name] = definition;}

expression lookup(string &name,environment &env)
{ 
 environment::iterator i = env.find(name);
 if(i != env.end()) return i->second;
 throw(string("not found"));
}
 
ostream &operator<<(ostream &o,const expression &e)
{
 vector<expression>::const_iterator i;
 switch(e.type)
 {
  case FREE_VARIABLE:  o << e.identifier; break;
  case BOUND_VARIABLE: o << *(e.reference); break;
  case NUMBER:         o << e.number; break;
  case LAMBDA:
  case LIST:
  case APPLICATION:    o << "(";
                       for(i=e.tokens.begin();
                           i!=e.tokens.end();i++)
                        o << " " << (*i);
                       o << " )";
  default:             break;
 }
 return o;
}

int isparen(char c) {return c == '(' || c== ')';}

void skipwhitespace(istream &in)
{
 char c = ' ';
 while(isspace(c) && !in.fail() && !in.eof()) c = in.get();
 if(!in.fail() && !in.eof()) in.unget();
}

istream &operator>>(istream &in,expression &expr)
{
 char c;
 string token;
 vector<expression> tokens;
 skipwhitespace(in); c = in.get();
 if(c==')')
 {
  cerr << "Error: in expression: mismatched parenthesis.\n";
  return in;
 }
 if(c=='(' && !in.fail() && !in.eof())
 {
  skipwhitespace(in); c = in.get();
  while(c!=')' && !in.fail() && !in.eof())
  {
   in.unget(); 
   tokens.push_back(expression());
   in >> tokens.back();
   skipwhitespace(in); c = in.get();
  }
  if(c!=')')
  {
   cerr << "Error: in expression: mismatched parenthesis.\n";
   return in;
  }
  expr = expression(tokens);
 }
 else
 {
  int quote = 0;
  int linecomment = 0;
  while(!in.fail() && !in.eof()
        && (quote || linecomment || (!isspace(c) && !isparen(c))) )
  {
   if(c=='`' && !quote)            quote = 1;
   else if(c=='\'' && quote)       quote = 0;
   else if(c==';')                 linecomment = 1;
   else if(c=='\n' && linecomment) {linecomment = 0; in.unget();}
   else if(!linecomment) token = token + c;
   c = in.get();
  }
  if(isparen(c)) in.unget();
  expr = expression(token);
 }
 return in;
}

expression evaluate(expression e,environment &env)
{
 vector<string>::iterator i;
 vector<expression>::iterator j;
 map<string, builtin>::iterator b;
 switch(e.type)
 {
  case FREE_VARIABLE:  try {e = evaluate(lookup(e.identifier,env),env);}
                       catch(string error) { }
                       return e;
  case BOUND_VARIABLE: return evaluate(*(e.reference),env);
  case LIST:
  case NUMBER:
  case LAMBDA:         return e;
  case APPLICATION:
      e.tokens[0] = evaluate(e.tokens[0],env);
      // beta reduction
      if(e.tokens[0].type==LAMBDA && e.tokens.size()==2)
      {
       // for strict evaluation we would evaluate e.tokens[1] first
       e.tokens[0].tokens[1].tokens[0] = e.tokens[1];
       return evaluate(e.tokens[0].tokens[2],env);
      }
      // multiple lambda applications using left associativity
      else if(e.tokens[0].type==LAMBDA && e.tokens.size()>2)
      {
       vector<expression> v(e.tokens.begin(),e.tokens.begin()+2);
       e.tokens[1] = expression(v);
       e.tokens.erase(e.tokens.begin());
       return evaluate(e,env);
      }
      // builtin expression
      else if(e.tokens[0].type==FREE_VARIABLE
              && (b=builtin_commands.find(e.tokens[0].identifier))
                  !=builtin_commands.end())
       return evaluate((b->second)(e,env),env);
      else cerr << "Error: Invalid application:\n"
                << e.tokens[0] << endl;
      return expression("#error");
  default: break;
 }
 return expression("#ERROR");
}

void interpreter(istream &in,environment &env)
{
 expression expr;
 while(!in.fail() && !in.eof())
 { in >> expr; evaluate(expr,env); }
}
#endif
