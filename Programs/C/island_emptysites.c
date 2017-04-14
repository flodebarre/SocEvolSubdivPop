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

#define BIRTHBASELINE ((double) 1.0) // Baseline fecundity
#define DEATHPROBA ((double) 0.1)   // Baseline death probability

#define DEMESIZE 5 // Number of individuals within each deme
#define NDEMES 50   // Number of demes

#define MIGRATION ((double) GGGG) // Emigration probability

#define noselfinteraction 1  // 1 if there are no benefits provided to self (strict pair interactions),
                             // 0 if there are (public good shared among all group members).

/* Other parameters */
int NNODES; // Total number of individuals

int popTot[NDEMES]; // Population vector, counting the total number of individuals in each deme
int popA[NDEMES]; // Population vector, counting the number of A individuals in each deme
int popA[NDEMES]; // Population vector, counting the number of B individuals in each deme

double fecundityA[NDEMES]; // Vector of total fecundities of A individuals in each deme
double fecundityB[NDEMES]; // Vector of total fecundities of B individuals in each deme

double mutation; // Mutation probability (in case we want to scale it)

int nA; // Current Number of type-A individuals in the population
int nB; // Current Number of type-B individuals in the population

int oldtype; // Save the old type of the replaced individual
int newtype; // New type of the replaced individual

int nodeD, nodeB; // Index of the nodes that die/reproduce in a given step

int savepopTot[(DEMESIZE*NDEMES)+1]; // Vector to save results as histogram (0, 1, ..., NNODES), total population size
int savepopA[(DEMESIZE*NDEMES)+1]; // Vector to save results as histogram (0, 1, ..., NNODES), number of A
int savepopB[(DEMESIZE*NDEMES)+1]; // Vector to save results as histogram (0, 1, ..., NNODES), number of B


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
double cumBA; // Sum of fecundities of A individuals
double cumBB; // Sum of fecundities of B individuals

double cumD; // Sum of all death rates
double cumDA; // Sum of death rates of A individuals
double cumDB; // Sum of death rates of B individuals

double migproba; // Dispersal probability to/from focal deme

double accessiblesites; // Where reproduction can occur

/* FUNCTIONS USED IN THE SCRIPT */
void GlobalInit(void);  // Global initializations (once)
void InitSim(void);     // Initializations for each simulation

void ChooseD(void);     // Choose who dies
void ChooseB(void);     // Choose who reproduces
void UpdatePop(void);   // Update the population

void OneStep(void);     // One simulation step

void ComputeRates(void); // Compute the probabilities of each type of event


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
    // Store the outcome of this chunk of time in histograms
    savepopA[nA] += 1;
    savepopB[nB] += 1;
  }

  /* OUTPUT: Print the result */
  for (iindiv=0; iindiv<NNODES+1; iindiv++) {
    printf("%d ", savepopA[iindiv]);
  }
  printf("\n");
  for (iindiv=0; iindiv<NNODES+1; iindiv++) {
    printf("%d ", savepopB[iindiv]);
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
  unsigned long seed=123456789 + randseed;
  srand48(seed);

  /* Set scaled parameters */
  NNODES = DEMESIZE * NDEMES; // Max population size
  mutation = SMUTATION; // Mutation

  /* Initiate savepop vectors */
  for (iindiv=0; iindiv<(NNODES+1); iindiv++){
    savepopA[iindiv] = 0;
    savepopB[iindiv] = 0;
  }
}


/* INITIALIZATIONS FOR EACH SIMULATION */
void InitSim(void)
{
  /* Initiate population */
  for(ideme=0; ideme<NDEMES; ideme++)
  {
    // Initialize deme composition
    popA[ideme] = 0;
    popB[ideme] = 0;
    // Then for all sites within the deme, choose one type (empty, A, B)
    for(iindiv=0; iindiv<DEMESIZE; iindiv++){
      double randpop = drand48();
      if (randpop < 0.333333){
        popA[ideme] += 1;
      }
      if (randpop > 0.666666){
        popB[ideme] += 1;
      }
    }
  }

  /* Compute the summary variables */
  nA = 0;
  nB = 0;
  for(ideme = 0; ideme<NDEMES; ideme++)
  {
    nA += popA[ideme];
    nB += popB[ideme];
  }
}


/*
 -------------------------------------------
 SIMULATION
 -------------------------------------------
*/

void ComputeRates(void)
{
  /* DEATH */
  cumD = 1.0 * DEATHPROBA * (nA + nB);
  cumDA = 1.0 * DEATHPROBA * nA;
  cumDB = 1.0 * DEATHPROBA * nB;

  /* BIRTH */
  cumB = 0.0;
  cumBA = 0.0;
  cumBB = 0.0;

  for (ideme =0; ideme<NDEMES; ideme++)
  {
    accessiblesites = ( (1.0 - MIGRATION)* ((double)DEMESIZE - (double)popA[ideme] - (double)popB[ideme])/((double)DEMESIZE) + MIGRATION * ((double)NNODES - (double)DEMESIZE - ((double)nA - (double)popA[ideme]) - ((double)nB - (double)popB[ideme]))/((double)NNODES - (double)DEMESIZE)  );
    // A number of simulation choices are made here:
    // - between whom the benefits are divided, if they are -> all other sites;
    // - whether the focal receives a share of what they contribute -> NO.
    fecundityA[ideme] = (BIRTHBASELINE + OMEGA * (BENEFIT*((double)popA[ideme] - 1.0) - COST) )/(DEMESIZE - 1.0) * accessiblesites ;
    fecundityB[ideme] = (BIRTHBASELINE + OMEGA * (BENEFIT*(double)popA[ideme] ) )/(DEMESIZE - 1.0) * accessiblesites;

    cumBA += fecundityA[ideme];
    cumBB += fecundityB[ideme];
  }
  cumB = cumBA + cumBB;
}

void EventDeath(void)
{
  // Draw deme where death happens, uniformly at random
  randnum = drand48() * cumD; // Draw random number

  cumsum = 0.0; // Initialize cumulative sum of rates
  for(ideme=0; ideme<NDEMES; ideme++) // For each deme,
  {
    cumsum += 1.0; // Update the cumulative sum up to ideme
    if(randnum < cumsum) // If we pick this deme
    {
      nodeD = ideme; // Save the deme index
      break;         // and stop the loop since we have the node we want
    }
  }

  // Draw ID of who dies:
  // pick one uniformly at random among alive individuals in the deme,
  // and update the population.
  cumD = ((double) popA[nodeD] + (double) popB[nodeD]);
  randnum = drand48() * cumD;

  if(randnum < ((double) popA[nodeD]) ){
    // Dead A
    popA[nodeD] += -1;
    nA += -1;
  }else{
    // Dead B
    popB[nodeD] += -1;
    nB += -1;
  }
}


void EventBirth(void)
{
  randnum = drand48() * cumB; // Draw random number

  if(randnum < cumBA)
  { // Reproduction of A individual

  } else {
    // Reproduction of B individual
    randnum += - cumBA;
  }
  cumsum = 0.0; /// Initialize the cumulative sum of rates
  for(ideme=0; ideme<NDEMES; ideme++) // For each potential reproducer
  {
    cumsum += fecundityA[ideme]; // Update the cumulative sum (A)
    if (randnum < cumsum) { // If we pick this node
      nodeB = ideme; // Index of the reproducer
      newtype = 1; // ID
      break;         // and stop the loop since we have the node we want
    }
    cumsum += fecundityB[ideme]; // Update the cumulative sum (B)
    if (randnum < cumsum) { // If we pick this node
      nodeB = ideme; // Index of the reproducer
      newtype = 0;  // ID
      break;         // and stop the loop since we have the node we want
    }
  }
}


void OneStep(void)
{
  // Choose event
  randnum = drand48() * (cumDA + cumDB + cumBA + cumBB);
  if(randnum < (cumDA + cumDB))
  {
    EventDeath();
  } else {
    // Rescale randnum for Birth events
    randnum += -(cumDA + cumDB);
  }
  ComputeRates();
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
