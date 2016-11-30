#! /bin/bash
##### NOT USED FOR THE MOMENT
#
# This script submits all the jobs for all simulations in the manuscript.
#

# Submit simulations for Island-structured populations
# pairwise, noself interactions
./submit_Island-BDDBWF.sh

# Submit simulations for Island-structured populations,
# common-good type of interactions
./submit_SI_Island-BDDBWF.sh
