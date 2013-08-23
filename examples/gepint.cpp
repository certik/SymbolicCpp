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


// gepint.cpp

#include <cstdlib>
#include <ctime>
#include <cmath>
#include <iostream>
#include <string>
#include "symbolicc++.h"
using namespace std;

const int nsymbols = 9;
// 3 terminal symbols (no arguments) x, 0 and 1
const int terminals = 4;
// terminal symbols first
const char symbols[nsymbols] = {'0','1','2','x','+','-','*','/','e'};
const int n = 2;   // for +,-,* and / which take 2 arguments
int h = 5;

Symbolic evalr(char *&e)
{
 switch(*(e++))
 {
  case '0': return 0.0;
  case '1': return 1.0;
  case 'x': return Symbolic("x");
  case '+': return evalr(e) + evalr(e);
  case '-': return evalr(e) - evalr(e);
  case '*': return evalr(e) * evalr(e);
  case '/': return evalr(e) / evalr(e);
  case 'e': return exp(evalr(e));
  default : return 0;
 }
}

Symbolic eval(char *e)
{ char *c = e; return evalr(c); }

void printr(char *&e)
{
 switch(*(e++))
 {
  case '0': cout << '0'; break;
  case '1': cout << '1'; break;
  case 'x': cout << 'x'; break;
  case '+': cout << '('; printr(e); cout << '+'; printr(e); cout << ')';
            break;
  case '-': cout << '('; printr(e); cout << '-'; printr(e); cout << ')';
            break;
  case '*': cout << '('; printr(e); cout << '*'; printr(e); cout << ')';
            break;
  case '/': cout << '('; printr(e); cout << '/'; printr(e); cout << ')';
            break;
  case 'e': cout << "exp("; printr(e); cout << ')'; break;
 }
}

void print(char *e) 
{ cout << eval(e) << endl; }

double fitness(char *c,const Symbolic &integrand)
{
 double j;
 double sum = 0;
 Symbolic x("x");
 Symbolic f = eval(c);
 Symbolic d_f = df(f,x);
 
 for(j=-1;j<=1;j+=0.1)
  sum += fabs(double((d_f-integrand)[x==j,SymbolicConstant::e==exp(1.0)]));
 return sum;
}

// population of size P
// eps = accuracy required
void gep(const Symbolic &integrand,int P,double eps)
{
 int i,j,k,replace,replace2,rlen,rp;
 int t = h*(n-1)+1;
 int gene_len = h+t;
 int pop_len = P*gene_len;
 int iterations = 0;
 char *population = new char[pop_len];
 char *elim = new char[P];
 int toelim = P/2;
 double bestf,f;        // best fitness, fitness value
 double sumf = 0.0;     // sum of fitness values
 double pmutate = 0.1;  // probability of mutation
 double pinsert = 0.4;  // probability of insertion
 double precomb = 0.7;  // probability of recombination
 double r,lastf;        // random numbers and roulette wheel selection
 char *best = (char*)NULL; //best gene
 char *iter;            // iteration variable

 // initialize the population
 for(i=0;i<pop_len;i++)
  if(i%gene_len < h) population[i] = symbols[rand()%nsymbols];
  else population[i] = symbols[rand()%terminals];

 // initial calculations
 bestf = fitness(population,integrand);
 best = population;
 for(i=0,sumf=0.0,iter=population;i<P;i++,iter+=gene_len)
 {
  f = fitness(iter,integrand);
  sumf += f;
  if(f<bestf)
  { bestf = f; best = population+i*gene_len; }
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
    f = fitness(population+j*gene_len,integrand);
    if((lastf<=r) && (r<f+lastf))
    { elim[j] = 1; j = P; }
    lastf += f;
   }
  }

  for(i=0;i<pop_len;)
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
    if(fitness(c,integrand) < fitness(population+replace,integrand))
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
     if(fitness(c[k],integrand) < fitness(c[j],integrand))
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
   f = fitness(iter,integrand);
   sumf += f;
   if(f < bestf) 
   { bestf = f; best = population+i*gene_len; }
 print(population+i*gene_len);
  }
  iterations++;
 print(best);
 cout << endl;
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
 Symbolic x("x");
 srand(time(NULL)); //set the seed for the random number generator
 gep(x,100,0.1);
 cout << endl;
 gep(x*ln(x),100,0.1);
 cout << endl;
 return 0;
}
