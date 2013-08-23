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


// lisp.h

#ifndef __LISP_H
#define __LISP_H

#include <iostream>
#include "identity.h"
using namespace std;

// Abstract base class
class Element
{
public:
   virtual Element *clone() const = 0;
   virtual void print(ostream&) const = 0;
   virtual int atom() const = 0;
   virtual ~Element() {}
};

//unique class for atoms nil and t
class __nil_and_t_lisp_class : public Element
{
 private:
         int is_t;
 public:
         __nil_and_t_lisp_class(int i=0) : is_t(i) {}
         void print(ostream &s) const {s<<((is_t)?"t":"nil");}
         Element *clone(void) const {return (Element*)this;}
         int atom(void) const {return 1;}
}nil(0),t(1);

class Pair : public Element  // Dotted pair
{
private:
   Element *_car;            // First element of the dotted pair
   Element *_cdr;            // Second element of the dotted pair

public:
   // Constructors
   Pair();
   Pair(const Element*);
   Pair(const Element*,const Element*);
   Pair(const Pair&);
   virtual ~Pair();

   Pair & operator = (const Pair&);

   // Member functions
   Element *car() const;
   Element *cdr() const;
  
   void car(const Element*);
   void cdr(const Element*);

   Element *clone() const;
   void print(ostream&) const;
   int atom() const;
};

template <class T>
class Type : public Element  // Atom
{
private:
   T thing;
  
public:
   // Constructors
   Type();
   Type(T);
   Type(const Type<T>&);

   // Member functions
   const T &value() const;

   Type<T> &operator = (const Type<T>&);
   Element *clone() const;
   void print(ostream&) const;
   int atom() const;
};

// Global functions
Pair *cons(const Element*,const Element*);
Element *append(const Element*,const Element*);
Element *car(const Element*);
Element *cdr(const Element*);

// Implementation

// class Pair
Pair::Pair() : _car(&nil), _cdr(&nil) {}

Pair::Pair(const Element *e) : _car(e->clone()), _cdr(&nil) {}

Pair::Pair(const Element *e1,const Element *e2) : 
  _car(e1->clone()), _cdr(e2->clone()) {}

Pair::Pair(const Pair &p)
{
   _car = &nil; _cdr = &nil;
   *this = p;
}

Pair::~Pair()
{
   //don't delete constants of the system
   if((_car != &nil)&&(_car != &t)) delete _car; _car = &nil;
   if((_cdr != &nil)&&(_cdr != &t)) delete _cdr; _cdr = &nil;
}

Pair &Pair::operator = (const Pair &p)
{
   if(this != &p)
   {
      if((_car != &nil)&&(_car != &t)) delete _car;
      if((_car != &nil)&&(_car != &t)) delete _cdr;

      _car = (p.car())->clone();
      _cdr = (p.cdr())->clone();
   }
   return *this;
}

Element *Pair::car() const
{ return _car; }

Element *Pair::cdr() const
{ return _cdr; }
  
void Pair::car(const Element *e)
{ if(e != &nil) _car = e->clone(); }

void Pair::cdr(const Element *e)
{ if(e != &nil) _cdr = e->clone(); }

Element *Pair::clone() const
{
   Pair *p = new Pair(*this);
   return p;
}

void Pair::print(ostream &os) const 
{ os << "(" << _car << " . " << _cdr << ")"; }

int Pair::atom() const { return 0; }

ostream &operator << (ostream &os,const Pair *p)
{
   if(p != NULL)
   { p->print(os); return os; }
   os << "nil";
   return os;
}

ostream &operator << (ostream &os,const Element *e)
{
   if(e != NULL) 
   {
      e->print(os);
      return os;
   }
   os << "nil";
   return os;
}

template <class T> Type<T>::Type() : thing(zero(T())) {}

template <class T> Type<T>::Type(T t) : thing(t) {}

template <class T> Type<T>::Type(const Type<T> &t)
{ thing = t.value(); }

template <class T>
Type<T> &Type<T>::operator = (const Type<T> &t)
{
   if(this != &t) thing = t.value();
   return *this;
}

template <class T> const T &Type<T>::value() const { return thing; }

template <class T> int Type<T>::atom() const { return 1; }

template <class T>
Element *Type<T>::clone() const
{
   Type<T> *t = new Type<T>(*this);
   return t;
}

template <class T> void Type<T>::print(ostream &os) const { os << thing; }

template <class T>
ostream &operator << (ostream &os,const Type<T> *t)
{
   t->print(os);
   return os;
}

Pair *cons(const Element *e1,const Element *e2)
{
   Pair *p = new Pair;
   p->car(e1);
   p->cdr(e2);
   return p;
}

Element *append(const Element *e1,const Element *e2)
{
   if(! e1->atom())
   {
      Pair *p = new Pair(*(Pair *)e1);
      Pair *e = p;
      while(e->cdr() != &nil) e = (Pair *)e->cdr();
      e->cdr(e2);
      return p;
   }
   cerr << "\nFirst argument of append must be a list" << endl;
   return &nil;
}

Element *car(const Element *e) 
{
   if(! e->atom()) return ((Pair*)e)->car();
   cerr << "\ncar: cannot take car of an atom!" << endl;
   return &nil;
}

Element *cdr(const Element *e)
{
   if(! e->atom()) return ((Pair*)e)->cdr();
   cerr << "\ncdr: cannot take cdr of an atom!" << endl;
   return &nil;
}

Element *null(Element *e)
{
 if(e==&nil) return &t;
 return &nil;
}

int is_lisp_list(Element *l)
{
 if(l==&nil) return 1;
 if(l->atom()) return 0;
 return is_lisp_list(((Pair*)l)->cdr());
}

int is_nonempty_lisp_list(Element *l)
{
 if(l->atom()) return 0;
 return is_lisp_list(((Pair*)l)->cdr());
}

Element *cond(Element *e)
{
 if(is_nonempty_lisp_list(e))
 {
  Element *firstcase=((Pair*)e)->car();
  if(is_nonempty_lisp_list(firstcase))
  {
   Element *condition=((Pair*)firstcase)->car();
   if(condition->atom())
   {
    if(condition==&nil)
     return cond(((Pair*)e)->cdr());
    else if(condition==&t)
     return ((Pair*)firstcase)->cdr();
    else cerr<<"cond expects a case first element to be nil or t."<<endl;
   }
   else cerr<<"cond expects a case first element to be nil or t."<<endl;
  }
  else cerr<<"cond expects a list for each case."<<endl;
 }
 else cerr<<"cond expects a list for evaluation."<<endl;
 return &nil;
}
#endif
