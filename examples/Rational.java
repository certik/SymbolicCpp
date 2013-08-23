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


// Rational.java

import java.lang.*;

class Rational extends Number 
{	
  private long num;	
  private long den;	

public Rational(long num,long den) { this.num = num; this.den = den; }	

private void normalize() 
{		
   long num = this.num;		
   long den = this.den;
   if(den < 0) { num = (-1)*num; den = (-1)*den; }	
}	

private void reduce() 
{		
   this.normalize();		
   long g = gcd(this.num,this.den);		
   this.num /= g;		
   this.den /= g;	
}	

private long gcd(long a,long b) 
{
   long g;
   if(b == 0) { return a; } 
   else 
   {			
   g = gcd(b,(a%b));
   if(g < 0) return -g;
   else return g;
   }	
}	

public long num() { return this.num; }	
	
public long den() { return this.den; }	
	
public void add(long num,long den) 
{		
   this.num = (this.num*den)+(num*this.den);
   this.den = this.den*den;		
   this.normalize();	
}	
	
public void add(Rational r) 
{		
   this.num = (this.num*r.den())+(r.num()*this.den);
   this.den = this.den*r.den();        
   this.normalize();	
}	
	
public void subtract(long num,long den) 
{		
   this.num = (this.num*den)-(num*this.den);
   this.den = this.den*den;
   this.normalize();	
}	
	
public void subtract(Rational r) 
{		
   this.num = (this.num*r.den())-(r.num()*this.den);
   this.den = this.den*r.den();
   this.normalize();	
}	
	
public void multiply(long num,long den) 
{		
   this.num = (this.num*num);
   this.den = (this.den*den);
   this.normalize();	
}	
	
public void multiply(Rational r) 
{		
   this.num = (this.num*r.num());		
   this.den = (this.den*r.den());		
   this.normalize();	
}	
	
public void divide(long num,long den) 
{		
   this.num = (this.num*den);
   this.den = (this.den*num);	
   this.normalize();	
}	

public void divide(Rational r) 
{		
   this.num = (this.num*r.den());
   this.den = (this.den*r.num());		
   this.normalize();	
}	

public static boolean equals(Rational a,Rational b) 
{		
   if((a.num()*b.den()) == (b.num()*a.den())) { return true; } 
   else { return false; } 
}	
	
public boolean equals(Object a) 
{		
   if(!(a instanceof Rational)) { return false; }
   return equals(this,(Rational) a);	
}	

public Object clone() { return new Rational(num,den); }

public String toString() 
{		
   StringBuffer buf = new StringBuffer(32);		
   long num, den, rem;		
   this.reduce();
   num = this.num;
   den = this.den;		
   if(num == 0) return "0";
   if(num == den) return "1";
   if(num < 0) 
   {			
    buf.append("-");
    num = -num;		
   }		
   rem = num%den;
   if(num > den) 
   {			
    buf.append(String.valueOf(num/den));
    if(rem == 0) { return buf.toString(); }
    else { buf.append(" "); }
   }
   buf.append(String.valueOf(rem));
   buf.append("/");
   buf.append(String.valueOf(den));
   return buf.toString();	
}	
	
public float floatValue() 
{ return (float) ((float)this.num/(float)this.den); }
	
public double doubleValue() 
{ return (double) ((double)this.num/(double)this.den); }	

public int intValue() 
{ return (int) ((int)this.num/(int)this.den); }	

public long longValue() 
{ return (long) ((long)this.num/(long)this.den); }	
	
public void print() 
{ System.out.print(this.toString()); }	

public void println() 
{ System.out.println(this.toString()); }	

// main() method used for testing other methods.	
public static void main(String args[]) 
{	
   Rational r1 = new Rational(-4,6);
   Rational r2 = new Rational(13,6);
   r1.add(r2);
   System.out.println(r1.toString());

   Rational r3 = new Rational(123,236);
   Rational r4 = new Rational(-2345,123);
   r3.multiply(r4);
   System.out.println(r3.toString());

   Rational r5 = new Rational(3,6);
   Rational r6 = new Rational(1,2);
   boolean b1 = equals(r5,r6);
   System.out.println("b1 = " +b1);
   boolean b2 = equals(r4,r5);
   System.out.println("b2 = " +b2);

   Rational r7 = new Rational(3,17);
   Rational r8 = (Rational) r7.clone();
   System.out.println("r8 = " + r8.toString()); 
}  // end main
}  // end class Rational
