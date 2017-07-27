
/*
 --------------------------------------------------------------
 SIMULATION STEP, WRIGHT-FISHER UPDATING
 This is a partial script; it will be later
 appended to the main script `htg-island_base.c`
 --------------------------------------------------------------
 */


void OneStep(void)
{
  /* Initialize new population measures */
  newn1 = 0;
  for(ideme=0; ideme<NDEMES; ideme++)
  {
    newpop[ideme] = 0;
  }

  /* Compute all fecundities in the population */
  // Compute fecundities for all individuals in the population
  for(ideme=0; ideme<NDEMES; ideme++) // For each deme
  {
    fecundityA[ideme] = (1.0 - OMEGA * COST + OMEGA * BENEFIT * (pop[ideme] - noselfinteraction)/(demesize[ideme] - noselfinteraction)) * pop[ideme]; // For A
    fecundityB[ideme] = (1.0 + OMEGA * BENEFIT * pop[ideme] /(demesize[ideme] - noselfinteraction)) * (demesize[ideme] - pop[ideme]); // For B
  }
  /* Everyone dies at each time step, so loop on each node in the population */
  for(nodeD=0; nodeD<NDEMES; nodeD++) // For each deme
  {
    // Compute the sum of fecundities among the neighbors, weighted by the probability that their offspring end up in the nodeD deme
    cumB = 0.0;
    for(jdeme=0; jdeme<NDEMES; jdeme++) // for all demes where the offspring come from
    {
      if(jdeme==nodeD){ // Migration depends on whethere we are considering nodeD or any other node
        migproba = ((double) (1.0-MIGRATION));
      }else{
        migproba = ((double) demesize[nodeD] * MIGRATION /(totpopsize - demesize[jdeme]) ); // Proba of landing to nodeD when you come from jdeme
      }

      tmpfecA[jdeme] = migproba * fecundityA[jdeme]; // For A
      tmpfecB[jdeme] = migproba * fecundityB[jdeme]; // For B

      cumB += tmpfecA[jdeme] + tmpfecB[jdeme];

    }

    // For each position in nodeD, look for replacement
    for(iindiv=0; iindiv<demesize[nodeD]; iindiv++)
    {
      cumsum = 0.0; // Initialize the cumulative sum of rates
      randnum = drand48() * cumB; // Draw a random number

      for(jdeme=0; jdeme<NDEMES; jdeme++) // For each deme with potential reproducer
      {
        cumsum += tmpfecA[jdeme]; // Update the cumulative sum (A)
        if (randnum < cumsum) { // If we pick this node
          nodeB = jdeme; // Index of the reproducer
          newtype = 1; // ID
          break;         // and stop the loop since we have the node we want
        }
        cumsum += tmpfecB[jdeme]; // Update the cumulative sum (B)
        if (randnum < cumsum) { // If we pick this node
          nodeB = jdeme; // Index of the reproducer
          newtype = 0;  // ID
          break;         // and stop the loop since we have the node we want
        }
      }

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
      newpop[nodeD] += newtype; // Update new-population vector: add the value of the new individual
      newn1 += newtype;        // Update new-n1

    } // end of iindiv loop
  } // end of the nodeD loop

  /* UPDATE THE POPULATION: current <- new */
  n1 = newn1;
  for (ideme=0; ideme<NDEMES; ideme++) {
    pop[ideme] = newpop[ideme];
  }
}
