#include <stdlib.h> /* to call drand48() */
#include <stdio.h>  /* to use printf() */
#include <math.h>
#include <time.h>


#define MEANDEMESIZE 3 // Average number of individuals per deme on top of focal
#define VARDEMESIZE 0.75 // Variance of the number of individuals per deme
#define NBINOM 4 // Binomial parameter such that n p = m (other combination is n=5, p=0.8)
#define NDEMES 30   // Number of demes

int demesize[NDEMES]; // Vector of deme sizes
double pbinom; // Parameter to generate deme sizes (p of Binomial)
int totsize; // Total population sizes

double randnum;

int ideme, jdeme; // Counters of demes
int iindiv; // Counter of individuals

void GenerateDemeSizes(void);


int main(void)
{
  GenerateDemeSizes();

  /* OUTPUT: Print the result */
  for (ideme=0; ideme<NDEMES; ideme++) {
    printf("%d ", demesize[ideme]);
  }
  printf("\n");
  printf("%d\n", totsize);
  printf("\n");
  return(0);
}

void GenerateDemeSizes(void)
{
  /* Define the parameters of the Binomial distribution used to draw deme sizes */
  pbinom = ((double) (MEANDEMESIZE - VARDEMESIZE)/MEANDEMESIZE);
  for(ideme = 0; ideme<NDEMES; ideme++){
    // For each deme, initialize deme size, and then draw number
    // Put at least one individual in each deme
    demesize[ideme] = 1;
    for(iindiv = 0; iindiv<NBINOM; iindiv++){
      // Consider each individual and see whether it goes to the focal deme
      // This means that we draw WITH REPLACEMENT but it is fine.
      randnum = drand48();
      if(randnum < pbinom){
        demesize[ideme] += 1;
      }
    }
  }
  /* Compute total population size */
  totsize = 0;
  for(ideme = 0; ideme<NDEMES; ideme++){
    totsize += demesize[ideme];
  }
}
