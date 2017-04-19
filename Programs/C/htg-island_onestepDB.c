

/*
 -------------------------------------------
 DEATH BIRTH UPDATING
 This is a partial script; it will be later
 appended to the main script `islandbase.c`
 -------------------------------------------
 */

/* FIRST STEP: DEATH */
void ChooseD(void)
{
  // Draw deme where death happens, uniformly at random
  cumD = ((double) totpopsize); // Sum of all death rates
  randnum = drand48() * cumD; // Draw random number

  cumsum = 0.0; // Initialize cumulative sum of rates
  for(ideme=0; ideme<NDEMES; ideme++) // For each node,
  {
    cumsum += demesize[ideme]; // Update the cumulative sum up to inode
    if(randnum < cumsum) // If we pick this node
    {
      nodeD = ideme; // Save the deme index
      break;         // and stop the loop since we have the node we want
    }
  }

  // Draw ID of who dies: pick one uniformly at random
  cumD = ((double) demesize[nodeD]);
  randnum = drand48() * cumD;

  if(randnum < pop[nodeD]){
    oldtype = 1; // Dead A
  }else{
    oldtype = 0; // Dead B
  }
}


/* SECOND STEP: BIRTH */
void ChooseB(void)
{
  /* Compute the cumulated fecundities of the neighbors, weighted by immigration probability */
  cumB = 0.0;
  for(ideme=0; ideme<NDEMES; ideme++) // For each deme
  {
    if(ideme==nodeD){
      migproba = ((double) (1.0-MIGRATION));
    }else{
      migproba = ((double) demesize[nodeD] * MIGRATION /(totpopsize - demesize[ideme]) ); // Proba to land in nodeD deme coming from ideme
    }

    fecundityA[ideme] = migproba * (1.0 - OMEGA * COST + OMEGA * BENEFIT * (pop[ideme] - noselfinteraction)/(demesize[ideme] - noselfinteraction)) * pop[ideme]; // For A
    fecundityB[ideme] = migproba * (1.0 + OMEGA * BENEFIT * pop[ideme] /(demesize[ideme] - noselfinteraction)) * (demesize[ideme] - pop[ideme]); // For B

    cumB += fecundityA[ideme] + fecundityB[ideme];

  }

  randnum = drand48() * cumB; // Draw random number

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




/*
 --------------------------------------------------------------
 UPDATE THE POPULATION ONCE nodeB and nodeD HAVE BEEN CHOSEN
 --------------------------------------------------------------
 */

/* UPDATE THE POPULATION */
void UpdatePop(void)
{
  /* UPDATE THE POPULATION */

  /* Type of the offspring (possible mutation) */
  randnum = drand48();
  if(randnum < mutation){ // If the offspring mutates, newtype can change
    // Draw another random number to determine its type
    randnum = drand48();
    if(randnum < P){ // Type 1 (more likely when higher P)
      newtype = 1;
    }else{           // Type 0
      newtype = 0;
    }
  }

  pop[nodeD] += -oldtype + newtype; // Update the population
  n1 += -oldtype + newtype; // Update the counter of type-1 individuals
}



/* ONE DB STEP */
void OneStep(void)
{
  for(microstep=0; microstep<totpopsize; microstep++){ // In Moran model, 1 generation = N microupdates
    ChooseD();   // Get nodeD, the index of the dying node
    ChooseB();   // Get nodeB, the index of the reproducing node
    UpdatePop(); // Update the population
  }
}
