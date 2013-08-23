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



// mep.cpp

#include <cctype>
#include <cstdlib>
#include <cmath>
#include <iostream>
#include <map>
#include <sstream>
#include <string>
#include <vector>
using namespace std;

typedef double (*op)(vector<double>);

// assume appropriate number of arguments
double add(vector<double> v)    { return v[0]+v[1]; }
double sub(vector<double> v)    { return v[0]-v[1]; }
double mul(vector<double> v)    { return v[0]*v[1]; }
double sine(vector<double> v)   { return sin(v[0]); }
double cosine(vector<double> v) { return cos(v[0]); }

string show(vector<string> e,string s = "")
{
  size_t i = 0, j = 0;
  string r;
  vector<string> operands;

  if(s == "") return show(e,e.back());

  while(i < s.size() && j != string::npos)
  {
   j = s.find(',',i);
   operands.push_back(s.substr(i,j-i));
   if(j != string::npos) i = j + 1;
  }

  if(operands.size() == 1)
  {
   if(isdigit(operands[0][0]))
   {
    int in;
    istringstream is(operands[0]);
    is >> in;
    return show(e,e[in]);
   }
   else return operands[0];
  }

  r = show(e,operands[0]);
  r += "(";

  for(i=1;i<operands.size()-1;i++)
  {
   r += show(e,operands[i]);
   r += ",";
  }

  r += show(e,operands[i]);
  r += ")";
  return r;
}

double evaluate(vector<string> e, map<string,double> values,
                map<string,op> ops, string s="")
{
  size_t i = 0, j = 0;
  string r;
  vector<string> operands;
  vector<double> operand_values;

  if(s == "") return evaluate(e,values,ops,e.back());

  while(i < s.size() && j != string::npos)
  {
   j = s.find(',',i);
   operands.push_back(s.substr(i,j-i));
   if(j != string::npos) i = j + 1;
  }

  if(operands.size() == 1)
  {
   if(isdigit(operands[0][0]))
   {
    int i;
    istringstream is(operands[0]);
    is >> i;
    return evaluate(e,values,ops,e[i]);
   }
   else return values[operands[0]];
  }

  r = show(e,operands[0]);

  for(i=1;i<operands.size();i++)
   operand_values.push_back(evaluate(e,values,ops,operands[i]));
  return ops[operands[0]](operand_values);
}

int main(void)
{
  map<string,op>     ops;
  map<string,double> values;
  vector<string>     expression(11);

  // operators used and corresponding arity
  ops["+"] = add;
  ops["-"] = sub;
  ops["*"] = mul;
  ops["s"] = sine;

  // values for evaluation
  values["a"] = 3.0;
  values["b"] = 4.0;
  values["c"] = 5.0;
  values["d"] = 6.0;

  // (a*b) + (c*d) + a - b + sin(b)
  expression[0] = "a";
  expression[1] = "b";
  expression[2] = "c";
  expression[3] = "d";
  expression[4] = "*,0,1";
  expression[5] = "*,2,3";
  expression[6] = "+,4,5";
  expression[7] = "+,6,0";
  expression[8] = "-,7,1";
  expression[9] = "s,1";
  expression[10] = "+,9,8";

  cout << show(expression,"") << endl;
  cout << evaluate(expression, values, ops) << endl;
  return 0;
}
