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


// gepchaos.cpp

#include <cstdlib>
#include <ctime>
#include <cmath>
#include <iostream>
#include <string>
using namespace std;

const double pi = 3.1415927;
const int nsymbols = 5;
// 2 terminal symbols (no arguments) x and 1
const int terminals = 2;
// terminal symbols first
const char symbols[nsymbols] = {'1','x','+','-','*'};
const int n = 2;   // for +,- and * which take 2 arguments
int h = 10;

double evalr(char *&e,double x)
{
 switch(*(e++))
 {
  case '1': return 1.0;
  case 'x': return x;
  case 'y': return pi*x;
  case 'c': return cos(evalr(e,x));
  case 's': return sin(evalr(e,x));
  case '+': return evalr(e,x)+evalr(e,x);
  case '-': return evalr(e,x)-evalr(e,x);
  case '*': return evalr(e,x)*evalr(e,x);
  default : return 0.0;
 }
}

double eval(char *e,double x)
{
 char *c = e;
 return evalr(c,x);
}

void printr(char *&e)
{
 switch(*(e++))
 {
  case '1': cout << '1'; break;
  case 'x': cout << 'x'; break;
  case 'y': cout << "pi*x"; break;
  case 'c': cout << "cos(";
            printr(e);
            cout << ")";
            break;
  case 's': cout << "sin(";
            printr(e);
            cout << ")";
            break;
  case '+': cout << '(';
            printr(e);
            cout << '+';
            printr(e);
            cout << ')';
            break;
  case '-': cout << '(';
            printr(e);
            cout << '-';
            printr(e);
            cout << ')';
            break;
  case '*': cout << '(';
            printr(e);
            cout << '*';
            printr(e);
            cout<<')';
            break;
 }
}

void print(char *e) 
{ 
 char *c = e;
 printr(c);
}

double fitness(char *c,double *data,int N)
{
 double sum = 0.0;
 double d;
 
 for(int j=0;j<N;j++)
 {
  d=eval(c,data[3*j])-data[3*j+1];
  if(data[3*j+2] == 0) sum += fabs(d);
  else if(data[3*j+2] > 0) sum -= (d > 0.0)?0.0:d;
  else if(data[3*j+2] < 0) sum += (d < 0.0)?0.0:d;
 }
 return sum;
}

// N number of data points
// population of size P
// eps = accuracy required
void gep(double *data,int N,int P,double eps)
{
 int i,j,k,replace,replace2,rlen,rp;
 int t = h*(n-1)+1;
 int gene_len = h+t;
 int pop_len = P*gene_len;
 int iterations = 0;
 char *population = new char[pop_len];
 char *elim = new char[P];
 int toelim = P/2;
 double bestf, f;       // best fitness, fitness value
 double sumf = 0.0;     // sum of fitness values
 double pmutate = 0.1;  // probability of mutation
 double pinsert = 0.4;  // probability of insertion
 double precomb = 0.7;  // probability of recombination
 double r,lastf;        // random numbers and roulette wheel selection
 char *best = (char*)NULL; //best gene
 char *iter;            // iteration variable

 // initialize the population
 for(i=0;i < pop_len;i++)
  if(i%gene_len < h)
   population[i] = symbols[rand()%nsymbols];
  else
   population[i] = symbols[rand()%terminals];

 // initial calculations
 bestf = fitness(population,data,N);
 best = population;
 for(i=0,sumf=0.0,iter=population;i<P;i++,iter+=gene_len)
 {
  f = fitness(iter,data,N);
  sumf += f;
  if(f<bestf)
  {
   bestf = f;
   best = population+i*gene_len;
  }
 }

 while(bestf >= eps)
 {
  // reproduction
  // roulette wheel selection
  for(i=0;i<P;i++) elim[i] = 0;
  for(i=0;i<toelim;i++)
  {
   r = sumf*(double(rand())/RAND_MAX);
   lastf = 0.0;
   for(j=0;j<P;j++)
   {
    f = fitness(population+j*gene_len,data,N);
    if((lastf<=r) && (r<f+lastf))
    {
     elim[j] = 1;
     j = P;
    }
    lastf += f;
   }
  }

  for(i=0;i < pop_len;)
  {
   if(population+i == best) i += gene_len; //never modify/replace best gene
   else for(j=0;j<gene_len;j++,i++)
   {
    // mutation or elimination due to failure in selection
    // for reproduction
    if((double(rand())/RAND_MAX < pmutate) || elim[i/gene_len])
    if(i%gene_len < h) population[i] = symbols[rand()%nsymbols];
    else population[i] = symbols[rand()%terminals];
   }

   // insertion
   if(double(rand())/RAND_MAX < pinsert)
   {
    // find a position in the head of this gene for insertion
    //  -gene_len for the gene since we have already moved
    //  to the next gene
    replace = i-gene_len;
    rp = rand()%h;
    // a random position for insertion source
    replace2 = rand()%pop_len;
    // a random length for insertion from the gene
    rlen = rand()%(h-rp);
    // create the new gene
    char *c = new char[gene_len];
    // copy the shifted portion of the head
    strncpy(c+rp+rlen,population+replace+rp,h-rp-rlen);
    // copy the tail
    strncpy(c+h,population+replace+h,t);
    // copy the segment to be inserted
    strncpy(c+rp,population+replace2,rlen);
    // if the gene is fitter use it
    if(fitness(c,data,N) < fitness(population+replace,data,N))
     strncpy(population+replace,c,h);
    delete[] c;
   }

   // recombination
   if(double(rand())/RAND_MAX < precomb)
   {
    // find a random position in the gene for one point recombination
    replace = i-gene_len;
    rlen = rand()%gene_len;
    // a random gene for recombination
    replace2 = (rand()%P)*gene_len;
    // create the new genes
    char *c[5];
    c[0] = population+replace; 
    c[1] = population+replace2;
    c[2] = new char[gene_len];
    c[3] = new char[gene_len];
    c[4] = new char[gene_len];
    strncpy(c[2],c[0],rlen);
    strncpy(c[2]+rlen,c[1]+rlen,gene_len-rlen);
    strncpy(c[3],c[1],rlen);
    strncpy(c[3]+rlen,c[0]+rlen,gene_len-rlen);
    // take the fittest genes
    for(j=0;j<4;j++)
    for(k=j+1;j<4;j++)
     if(fitness(c[k],data,N) < fitness(c[j],data,N))
     {
      strncpy(c[4],c[j],gene_len);
      strncpy(c[j],c[k],gene_len);
      strncpy(c[k],c[4],gene_len);
     }
    delete[] c[2];
    delete[] c[3];
    delete[] c[4];
   }
  }

  // fitness
  for(i=0,sumf=0.0,iter=population;i<P;i++,iter+=gene_len)
  {
   f = fitness(iter,data,N);
   sumf += f;
   if(f < bestf) 
   {
    bestf = f;
    best = population+i*gene_len;
   }
  }
  iterations++;
 }

 print(best);
 cout << endl;
 cout << "Fitness of " << bestf << " after "
      << iterations << " iterations." << endl;

 delete[] population;
 delete[] elim;
}

int main(void)
{
 srand(time(NULL)); //set the seed for the random number generator

 double data[] = {0,0,0,1,0,0,0.5,1,0,0.25,0.5,1,0.75,0.5,1,
                  0.25,1,-1,0.75,1,-1};
 gep(data,7,30,0.001);
 cout << endl;

 return 0;
}
