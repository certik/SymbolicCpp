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


// anyset.cpp

#include <iostream>
#include <set>
#include <string>
#include <typeinfo>
using namespace std;

class Data
{
 public: virtual const type_info &type() const = 0;
         virtual ostream &print(ostream&) const = 0;
         virtual Data *copy() const = 0;
         virtual int operator<(const Data&) const = 0;
         virtual ~Data() {}
};

ostream &operator<<(ostream &o,const Data &d)
{ return d.print(o); }

template <class T>
class DataT: public Data
{
 private: T data;
 public: DataT(const T &t) : data(t) {}
         DataT(const DataT<T> &t) : data(t.data) {}
         virtual ~DataT() {}
         virtual const type_info &type() const { return typeid(T); }
         virtual ostream &print(ostream &o) const { return (o << data); }
         virtual Data *copy() const { return new DataT(*this); };
         virtual int operator<(const Data &d) const
         {
          if(type() == d.type()) return (data < ((const DataT *)&d)->data);
          return (type().name() < d.type().name());
         }
         const T &getdata() const { return data; }
};

class AnyData
{
 private: Data *data;
 public: template <class T> AnyData(const T &t) { data = new DataT<T>(t); }
         AnyData(const AnyData &a) { data = a.data->copy(); }
         ~AnyData() { delete data; }
         const type_info &type() const { return data->type(); }
         ostream &print(ostream &o) const { return (o << *data); }
         // for set implementation
         int operator<(const AnyData &a) const { return *data < *(a.data); }
         template <class T> operator T() const
         {
          if(type() != typeid(T)) throw string("Wrong type");
          return ((DataT<T> *)data)->getdata();
         }
};

ostream &operator<<(ostream &o,const AnyData &d)
{ return d.print(o); }

int main(void)
{
 set<AnyData> s;
 s.insert(2);
 s.insert(string("string"));
 s.insert(3.5);
 s.insert(2);
 set<AnyData>::iterator si;
 for(si=s.begin();si!=s.end();si++)
 {
  cout << *si << " of type " << si->type().name() << endl;
  if(si->type() == typeid(int) && int(*si) == 2)
   cout << "Found the integer value 2" << endl;
 }

 return 0;
}
