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


// interpreter.cpp

#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <map>
#include <cstdlib>
#include <cmath>
#include "symbolicc++.h"
using namespace std;

double error(string s)
{ cerr << "Error: " << s << ", using 0." << endl; return 0.0; }

class token
{
  private:
   int is_value;
   Symbolic v;
   string t;
   static map<string,Symbolic> values;
  public:
   token() : is_value(0), v(0), t("") {};
   token(const Symbolic &s) : is_value(1), v(s), t("") {};
   token(const string &s) : is_value(0), v(0), t(s)  {};
   Symbolic value();
   string name() { return t; }
   int isvalue() { return is_value; }
   Symbolic set(const Symbolic &d) { return values[t]=d; }
   int operator==(string s) { return (!is_value) && (t == s); }
   friend ostream& operator << (ostream&,token);
};

map<string,Symbolic> token::values;

Symbolic token::value()
{
  if(is_value) return v;
  char *end;
  int vali=(int)strtol(t.c_str(),&end,10);
  if(*end == '\0') return Symbolic(vali);
  double vald=strtod(t.c_str(),&end);
  if(*end == '\0') return Symbolic(vald);
  if(values.count(t)>0) return values[t];
  return Symbolic(t);
}

ostream& operator << (ostream& o,token t)
{ if(t.is_value) o << t.v; else o << t.t; return o;}

vector<token>
get_tokens(string s,string separator[],string ignore[])
{
  int i = 0, j, istoken = 0;
  vector<token> v;
  string value = "";
  while(i<(int)s.length())
  {
   istoken = 0;
   for(j=0;ignore[j] != "" && i<(int)s.length();j++)
    if(s.substr(i,ignore[j].length()) == ignore[j])
     i += ignore[j].length(), j = -1, istoken = 1;
   for(j=0;separator[j] != "" && !istoken;j++)
    if(s.substr(i,separator[j].length()) == separator[j])
    {
     if(value != "") { v.push_back(token(value)); value = ""; }
     v.push_back(token(separator[j]));
     i += separator[j].length();
     istoken = 1;
    }
   if(!istoken) value += s[i++];
   else if(value!="") { v.push_back(token(value)); value = ""; }
  }
  if(value != "") v.push_back(token(value));
  return v;
}

Symbolic spow(const Symbolic &x,const Symbolic &y) { return (x^y); }
Symbolic smul(const Symbolic &x,const Symbolic &y) { return x*y;   }
Symbolic sdiv(const Symbolic &x,const Symbolic &y) { return x/y;   }
Symbolic sadd(const Symbolic &x,const Symbolic &y) { return x+y;   }
Symbolic ssub(const Symbolic &x,const Symbolic &y) { return x-y;   }

Symbolic ssqrt(const vector<Symbolic> &x)  { return sqrt(x[0]);           }
Symbolic scos(const vector<Symbolic> &x)   { return cos(x[0]);            }
Symbolic ssin(const vector<Symbolic> &x)   { return sin(x[0]);            }
Symbolic stan(const vector<Symbolic> &x)   { return tan(x[0]);            }
Symbolic sexp(const vector<Symbolic> &x)   { return exp(x[0]);            }
Symbolic sln(const vector<Symbolic> &x)    { return ln(x[0]);             }
Symbolic slog(const vector<Symbolic> &x)   { return log(x[0],x[1]);       }
Symbolic sdf(const vector<Symbolic> &x)    { return df(x[0],x[1]);        }
Symbolic ssubst(const vector<Symbolic> &x) { return x[0].subst(x[1],x[2]);}
Symbolic sfunc(const vector<Symbolic> &x)  { return x[0][x[1]];           }

Symbolic evaluate(token t);

struct function {
 string name;
 int    args;
 Symbolic (*impl)(const vector<Symbolic>&);
};

Symbolic evaluate_tokens(vector<token> v)
{
  vector<token> v2, v3;
  int parenthesis, i, j, k;
  // function names, arity, and their implementation
  function functions[] = {
   { "sqrt",     1, ssqrt  },
   { "cos",      1, scos   },
   { "sin",      1, ssin   },
   { "tan",      1, stan   },
   { "exp",      1, sexp   },
   { "ln",       1, sln    },
   { "log",      2, slog   },
   { "df",       2, sdf    },
   { "subst",    3, ssubst },
   { "function", 2, sfunc  },
   { "" } };

  // default left operands for binary operators
  double initleft[] = { 1.0, 1.0, 0.0 };
  // binary operators and their implementation
  string opnames[][4] = { { "^", "" }, { "*", "/", "" }, { "+", "-", "" } };
  Symbolic (*opimpl[][3])(const Symbolic&,const Symbolic&) =
     { { spow }, { smul, sdiv }, { sadd, ssub } };

  // check for the assignment statement
  if(v.size()>2 && v[1] == "=") {
    for(j=2;j<(int)v.size();j++) v2.push_back(v[j]);
    return v[0].set(evaluate_tokens(v2));
  }

  // evaluate parenthesis first
  for(j=0;j<(int)v.size();j++)
  {
   if(v[j] == ")") return error("unbalanced parenthesis");
   else if(v[j] == "(")
   {
    for(parenthesis=1,j++;parenthesis && j<(int)v.size();j++)
    {
     if(v[j] == "(") parenthesis++;
     if(v[j] == ")") parenthesis--;
     // artificially end the parenthesized expression
     if(v[j] == "," && parenthesis == 1)
     {
      v2.push_back(token(evaluate_tokens(v3)));
      v3.clear();
     }
     else if(parenthesis) v3.push_back(v[j]);
    }
    if(parenthesis) return error("unbalanced parenthesis");
    v2.push_back(token(evaluate_tokens(v3)));
    v3.clear(); j--;
   }
   else v2.push_back(v[j]);
  }

  // evaluate functions
  for(j=0,v.clear();j<(int)v2.size();j++)
  {
   for(i=0;functions[i].name!="";i++)
   if(v2[j] == functions[i].name)
   {
    if(j+functions[i].args<(int)v2.size())
    {
      vector<Symbolic> args;
      for(k=1;k<=functions[i].args;k++)
       args.push_back(evaluate(v2[j+k]));
      v.push_back(token(functions[i].impl(args)));
      j+=functions[i].args;
    }
    else return error(functions[i].name           +
                      " without "                 +
                      char('0'+functions[i].args) +
                      " arguments");
    break;
   }
   if(functions[i].name=="") v.push_back(v2[j]);
  }
  // evaluate operators in order of precedence
  for(k=0,v2.clear();k<3;k++,v = v2,v2.clear())
  {
   token left(initleft[k]);
   for(j=0;j<(int)v.size();j++)
   {
    for(i=0;opnames[k][i]!="";i++)
    if(v[j] == opnames[k][i])
    {
     if(v2.size()) v2.pop_back();
     if(j+1<(int)v.size())
      v2.push_back(token(opimpl[k][i](evaluate(left),
                                      evaluate(v[++j]))));
     else return error(opnames[k][i]+" without second argument");
     break;
    }
    if(opnames[k][i]=="") v2.push_back(v[j]);
    left = v2.back();
   }
  }
  // check that evaluation gave a single result
  if(v.size() != 1)
  {
   for(j=0;j<(int)v.size();j++)
    cerr << "token " << j+1 << " : " << v[j] << endl;
   return error("could not evaluate expression");
  }
  return v[0].value();
}

Symbolic evaluate(token t)
{ vector<token> v; v.push_back(t); return evaluate_tokens(v); }

Symbolic evaluateformula(istream &s)
{
  char c;
  string expression;
  static string ws[] = { " ", "\t", "\n", "\r", "" };
  static string separator[] = { "=", "+", "-", "*", "/",
                                "^", "(", ")", ",", "" };
  do if((c = s.get()) != ';' && !s.eof()) expression += c;
  while(c != ';' && !s.eof());
  if(c != ';') return error("formula not terminated");
  vector<token> v = get_tokens(expression,separator,ws);
  return evaluate_tokens(v);
}

int main(void)
{
  while(!cin.eof())
  cout << " -> " << evaluateformula(cin) << endl;
  return 0;
}
