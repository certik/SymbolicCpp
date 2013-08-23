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


// builtin_io.h

#ifndef BUILTIN_IO_H
#define BUILTIN_IO_H

expression builtin_display(expression &e,environment &env)
{
 int space = 0;
 vector<expression>::iterator i;
 for(i=e.tokens.begin()+1;i!=e.tokens.end();i++)
 {
  if(space) cout << " ";
  cout << evaluate(*i, env);
  space = 1;
 }
 return expression("#display");
}

expression builtin_import(expression &e,environment &env)
{
 vector<expression>::iterator i;
 for(i=e.tokens.begin()+1;i!=e.tokens.end();i++)
 {
  expression e1 = evaluate(*i, env);
  if(e1.type==FREE_VARIABLE)
  {
   ifstream f(e1.identifier.c_str());
   if(f.fail())
    cerr << "Error: (import " << e1 << ") failed to open file\n";
   else interpreter(f,env);
   f.close();
  }
  else
   cerr << "Error: (import " << e1 << ") is not a filename\n";
 }
 return expression("#import");
}

expression builtin_newline(expression &e,environment &env)
{cout << endl; return expression("#newline");}
#endif
