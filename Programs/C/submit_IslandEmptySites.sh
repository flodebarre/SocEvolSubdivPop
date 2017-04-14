#! /bin/bash

#
# This script submits jobs for the simulations on island-structured populations,
# with empty sites and a birth-death process (not Moran)
#

export THEPATH=SocEvolSubdivPop/

for mutt in 0.001 0.01 0.1 0.25
do
for mB in 15.0
do
for mp in 0.45
do
for mig in 0.025 0.075 0.15 0.3 0.4 0.125 0.175 0.25 0.35 0.45
do
for omega in 0.05
do

# Change parameters
#  Change the mp parameter
sed -e "s/XXXX/${mp}/" -e "s/NREP 10000000/NREP 1000000/" -e "s/BBBB/${mB}/" -e "s/MMMM/${mutt}/" -e "s/GGGG/${mig}/" -e "s/OOOO/${omega}/" island_emptysites.c > Scripts/EmptySites_${mig}_${mB}_${mp}_${mutt}_${omega}.c

# Compile the script
cc Scripts/EmptySites_${mig}_${mB}_${mp}_${mutt}_${omega}.c -o Scripts/EmptySites_${mig}_${mB}_${mp}_${mutt}_${omega} -lm

# Create the execution file (two lines)
echo -e "#!/bin/bash\n./${THEPATH}Scripts/EmptySites_${mig}_${mB}_${mp}_${mutt}_${omega} > ${THEPATH}Results/EmptySites_${mig}_${mB}_${mp}_${mutt}_${omega}.txt" > Scripts/EmptySites_${mig}_${mB}_${mp}_${mutt}_${omega}.sh

# Make the execution files executable
chmod +x Scripts/EmptySites_${mig}_${mB}_${mp}_${mutt}_${omega}.sh

# Submit the job (-q long.q)
qsub -q long.q Scripts/EmptySites_${mig}_${mB}_${mp}_${mutt}_${omega}.sh

#---------------------------------------------------------------------------------------------------------
done # omega
done # mig
done # mB
done # mp
done # mutt
