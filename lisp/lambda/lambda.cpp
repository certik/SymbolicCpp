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


// lambda.cpp

#include <iostream>
#include <fstream>
#include <sstream>
#include "expression.h"
#include "builtin_arith.h"
#include "builtin_core.h"
#include "builtin_io.h"
#include "builtin_list.h"
#include "builtin_math.h"
using namespace std;

environment global;

int main(int argc,char *argv[])
{
 stringstream s;

 // core
 builtin_commands["apply"]     = builtin_apply;
 builtin_commands["define"]    = builtin_define;
 builtin_commands["defined?"]  = builtin_definedp;
 builtin_commands["sequence"]  = builtin_sequence;
 builtin_commands["sequence*"] = builtin_sequence_star;
 // io
 builtin_commands["display"]   = builtin_display;
 builtin_commands["import"]    = builtin_import;
 builtin_commands["newline"]   = builtin_newline;
 // lists
 builtin_commands["car"]       = builtin_car;
 builtin_commands["cdr"]       = builtin_cdr;
 builtin_commands["cons"]      = builtin_cons;
 builtin_commands["empty?"]    = builtin_emptyp;
 builtin_commands["list"]      = builtin_list;
 // arithmetic
 builtin_commands["+"]        = builtin_add;
 builtin_commands["-"]        = builtin_sub;
 builtin_commands["*"]        = builtin_mul;
 builtin_commands["/"]        = builtin_div;
 builtin_commands["%"]        = builtin_mod;
 builtin_commands["="]        = builtin_equality;
 builtin_commands[">"]        = builtin_greater;
 // math
 builtin_commands["sin"]      = builtin_sin;
 builtin_commands["cos"]      = builtin_cos;
 builtin_commands["exp"]      = builtin_exp;
 builtin_commands["log"]      = builtin_log;
 builtin_commands["sqrt"]     = builtin_sqrt;

 if(argc==1) interpreter(cin,global);
 else
 {
  s << "(import " << argv[1] << ")" << endl;
  interpreter(s,global);
 }
 return 0;
}
