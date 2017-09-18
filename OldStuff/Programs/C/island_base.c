#include <stdlib.h> /* to call drand48() */
#include <stdio.h>  /* to use printf() */
#include <math.h>
#include <time.h>

#define NREP 10000000 // Number of replicates
#define TIMEINTERVAL 1000 // Time between which measures are taken

#define SMUTATION ((double) MMMM )   // Mutation probability
#define P ((double) XXXX ) // Proba mutant is mutant

#define OMEGA ((double) OOOO)   // Strength of selection (baseline was 0.005)

#define BENEFIT ((double) (BBBB)) // Value of b in the interaction matrix
#define COST ((double) 1.0) // Value of c in the interaction matrix

#define DEMESIZE 3 // Number of individuals within each deme
#define NDEMES 4   // Number of demes

#define MIGRATION ((double) GGGG) // Emigration probability

#define noselfinteraction 1  // 1 if there are no benefits provided to self (strict pair interactions),
                             // 0 if there are (public good shared among all group members).

/* Other parameters */
int NNODES; // Total number of individuals

int pop[NDEMES]; // Population vector, counting the number of A individuals in each deme

double fecundityA[NDEMES]; // Vector of total fecundities of A individuals in each deme
double fecundityB[NDEMES]; // Vector of total fecundities of B individuals in each deme

double mutation; // Mutation probability (in case we want to scale it)

int n1; // Number of type-1 individuals in the population (1==A, 0==B)

int oldtype; // Save the old type of the replaced individual
int newtype; // New type of the replaced individual

int nodeD, nodeB; // Index of the nodes that die/reproduce in a given step

int savepop[(DEMESIZE*NDEMES)+1]; // Vector to save results as histogram (0, 1, ..., NNODES)


// Only needed for the WF updating -----------------------|
int newn1; // Temporary n1                                |
int newpop[NDEMES]; // Temporary pop vector               |
double tmpfecA[NDEMES]; // Temporary fecundity vector (A) |
double tmpfecB[NDEMES]; // Temporary fecundity vector (B) |
// -------------------------------------------------------|

/* Simulation-specific stuff */
int step, replic; // Counters of steps and replicates
int ideme, jdeme; // Counters of demes
int iindiv; // Counter of individuals
int timestep;     // Time counter

// Only needed for Moran updating -------------------------------------|
int microstep; // Counter of steps in a generation in the Moran model  |
//---------------------------------------------------------------------|

double randnum, otherrandnum; // Random numbers
double cumsum;  // Cumulative sum

double cumB; // Sum of all fecundities
double cumD; // Sum of all death rates

double migproba; // Dispersal probability to/from focal deme

/* FUNCTIONS USED IN THE SCRIPT */
void GlobalInit(void);  // Global initializations (once)
void InitSim(void);     // Initializations for each simulation

void ChooseD(void);     // Choose who dies
void ChooseB(void);     // Choose who reproduces
void UpdatePop(void);   // Update the population

void OneStep(void);     // One simulation step


int main(void)
{
  /* Global initializations */
  GlobalInit();

  /* SIMULATIONS */
  /* Initiate the simulation */
  InitSim();

  /* Run it for NREP*TIMEINTERVAL generations, regularly saving the results */
  for(timestep=0; timestep<NREP; timestep++){ // For each big chunk of time
    for (step=0; step<TIMEINTERVAL; step++) {
      OneStep(); // Run simulation for TIMEINTERVAL generations
    }
    savepop[n1] += 1; // Store the outcome of this chunk of time in a histogram
  }

  /* OUTPUT: Print the result */
  for (iindiv=0; iindiv<NNODES+1; iindiv++) {
    printf("%d ", savepop[iindiv]);
  }
  printf("\n");
  return(0);
}



/*
 ===========================================
 SIMULATION FUNCTIONS
 ===========================================
 */


/*
 -------------------------------------------
 INITIALIZATIONS
 -------------------------------------------
 */

/* GLOBAL INITIALIZATIONS (DO ONLY ONCE) */
void GlobalInit(void)
{
  /* Set random seed */
  // Based on the parameters for more variation
  int randseed = ((int)floor(10000*SMUTATION + 10*MIGRATION + 1000*BENEFIT + 100*P + NNODES));
  unsigned long seed=2101198411 + randseed;
  srand48(seed);

  /* Set scaled parameters */
  NNODES = DEMESIZE * NDEMES; // Total population size
  mutation = SMUTATION; // Mutation

  /* Initiate savepop */
  for (iindiv=0; iindiv<(NNODES+1); iindiv++){
    savepop[iindiv] = 0;
  }
}


/* INITIALIZATIONS FOR EACH SIMULATION */
void InitSim(void)
{
  /* Initiate population */
  // Uniform population initially, alternatively all-1 or all-0 (1==A, 0==B)
  // All population of type 1
  if (replic % 2) {
    for (ideme=0; ideme<NDEMES; ideme++)
    {
      pop[ideme] = DEMESIZE;
    }
    n1 = NNODES; // Number of mutants in the population

  // Otherwise all population of type 0
    } else {
    for (ideme=0; ideme<NDEMES; ideme++)
    {
      pop[ideme] = 0;
    }
    n1 = 0;     // Number of mutants in the population
  }
}




/*
 -----------------------------------------------------------------------------------
                **** NOTE ****

 The function `OneStep` depends on the updating rule;
 It is defined in another script, `islandonestepUR.c`,
 where UR is the two-letter name of the updating rule (WF, BD, DB).
 This other script is then appended to `islandbase.c`.

 -----------------------------------------------------------------------------------
 */
