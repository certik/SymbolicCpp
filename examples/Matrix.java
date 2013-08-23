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


// Matrix.java

class Matrix 
{  
   private int rows, columns;
   public double entries[][];
  
   Matrix(int m,int n)   // constructor
   { 
   rows = m;
   columns = n;
   entries = new double[m][n];
   for(int i=0;i<rows;i++)
   for(int j=0;j<columns;j++)
   entries[i][j] = 0.0;
   }
   
   Matrix(int m,int n,double[][] A) // constructor
   { rows = m; columns = n; entries = A; }
   
   public void add(Matrix M)
   {
   if((this.rows != M.rows) || (this.columns != M.columns))
   {
   System.out.println("matrices cannot be added");
   System.exit(0);
   }
   for(int i=0;i<columns;i++)
   for(int j=0;j<rows;j++)          
   this.entries[i][j] = this.entries[i][j] + M.entries[i][j]; 
   }

   public Matrix multiply(Matrix M) 
   {
   int i, j, t;
   if(columns != M.rows) 
   {
   System.out.println("matrices cannot be multiplied");
   System.exit(0);
   }
   Matrix product = new Matrix(rows,M.columns);
   for(i=0;i<rows;i++)
   {
   for(j=0;j<M.columns;j++)
   {
   double tmp = 0.0;
   for(t =0;t<columns;t++)
   tmp = tmp + entries[i][t]*M.entries[t][j];
   product.entries[i][j] = tmp;
   }
   }
   return product;
   }
   
   public void randomize() 
   {
   for(int i=0;i<rows;i++)
   for(int j=0;j<columns;j++)
   entries[i][j] = Math.random();
   }

   public boolean equals(Matrix A,Matrix B)
   {
   for(int i=0;i<rows;i++)
   {
   for(int j=0;j<columns;j++)
   {
   if(A.entries[i][j] != B.entries[i][j])
   return false;
   }
   }
   return true;
   }

   public boolean equals(Object ob)
   {
   if(!(ob instanceof Matrix)) { return false; }
   return equals(this,(Matrix) ob);
   }

   public Object clone() { return new Matrix(rows,columns,entries); }
 
   public String toString() 
   {
   String result = new String();
   for(int i=0;i<rows;i++)
   {
   for(int j=0;j<columns;j++)
   {
   result = result + String.valueOf(entries[i][j])+"   ";
   }
   result = result + "\n";
   }
   result = result + "\n";
   return result;
   }

   public void onStdout()
   {
   System.out.println(toString());
   }

   public static void main(String args[])
   {
   Matrix M = new Matrix(2,2);
   M.entries[0][0] = 3.4; M.entries[0][1] = 1.2;
   M.entries[1][0] = 4.5; M.entries[1][1] = 5.8;
   Matrix N = new Matrix(2,2);
   N.entries[0][0] = 6.4; N.entries[0][1] = -1.2;
   N.entries[1][0] = 8.5; N.entries[1][1] = 6.8;
   M.add(N);
   System.out.println("M = \n" + M.toString());
   Matrix X = new Matrix(2,2);
   X = M.multiply(N);
   System.out.println("X = \n" + X.toString());
   Matrix Y = new Matrix(2,2);
   Matrix Z = new Matrix(2,2);
   boolean b1 = Y.equals(Z);
   System.out.println("b1 = " +b1);
   Z.randomize();
   System.out.println("Z = \n" +Z);
   boolean b2 = Y.equals(Z);
   System.out.println("b2 = " +b2);
   double d[][] = new double[2][2];
   d[0][0] = 2.1; d[0][1] = -3.4;
   d[1][0] = 0.9; d[1][1] = 5.6;
   Matrix B = new Matrix(2,2,d);
   B.add(B);
   System.out.println("B = \n" + B.toString());
   Matrix U = (Matrix) X.clone();
   System.out.println("U = \n" + U.toString());   
   }
}
