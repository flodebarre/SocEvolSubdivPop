
/*
 -------------------------------------------
 BIRTH DEATH UPDATING
 This is a partial script; it will be later 
 appended to the main script `islandbase.c`
 -------------------------------------------
 */

/* FIRST STEP: BIRTH */
void ChooseB(void)
{
  // Compute fecundities for all individuals in the population, and the sum of all fecundities
  cumB = 0.0; // Initialize sum of fecundities

  for(ideme=0; ideme<NDEMES; ideme++) // For each focal node (inode)
  {
    fecundityA[ideme] = (1.0 - OMEGA * COST + OMEGA * BENEFIT * (pop[ideme] - noselfinteraction)/(DEMESIZE - noselfinteraction)) * pop[ideme]; // For A
    fecundityB[ideme] = (1.0 + OMEGA * BENEFIT * pop[ideme] /(DEMESIZE - noselfinteraction)) * (DEMESIZE - pop[ideme]); // For B

    cumB += fecundityA[ideme] + fecundityB[ideme]; // Increment the sum of fecundities
  }

  randnum = drand48() * cumB; // Draw random number
  
  cumsum = 0; // Initialize cumulative sum of rates
  for(ideme=0; ideme<NNODES; ideme++) // For each node,
  {
    cumsum += fecundityA[ideme]; // Update the cumulative sum up to the A individuals of the deme
    if(randnum < cumsum) // If we pick this node
    {
      nodeB = ideme; // Save the deme index
      newtype = 1;   // Save the ID of the parent (typeA = 1)
      break;         // and stop the loop since we have the node we want
    }
    cumsum += fecundityB[ideme]; // Update the cumulative sum up to all individuals of the deme
    if(randnum < cumsum) // If we pick this node
    {
      nodeB = ideme; // Save the deme index
      newtype = 0;   // Save the ID of the parent (typeB = 0)
      break;         // and stop the loop since we have the node we want
    }
  }
}

/* SECOND STEP: DEATH */
void ChooseD(void)
{
  cumD = 1.0; // The degree is 1 in this model
  // First, find the deme were death occurs
  randnum = drand48() * cumD; // Draw random number
  
  cumsum = 0.0;
  for(ideme=0; ideme<NDEMES; ideme++)
  {
    if(ideme==nodeB){
      cumsum += (1.0-MIGRATION);
    }else{
      cumsum += MIGRATION/(NDEMES-1.0);
    }
    if(randnum < cumsum){
      nodeD = ideme; // Save the deme index
      break;
    }
  }
  // Then, find the ID of the dead individual
  randnum = drand48()*DEMESIZE;
  if(randnum < pop[nodeD]){
    oldtype = 1; // A type-A individual died
  }else{
    oldtype = 0; // A type-B individual died
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
  
  pop[nodeD] += - oldtype + newtype; // Update the deme
  n1 += -oldtype + newtype; // Update the counter of type-1 individuals
}



/* ONE BD STEP */
void OneStep(void)
{
  for(microstep=0; microstep<NNODES; microstep++){ // In Moran model, 1 generation = N microupdates
    ChooseB();   // Get nodeB, the index of the reproducing node
    ChooseD();   // Get nodeD, the index of the dying node
    UpdatePop(); // Update the population
  }

}


