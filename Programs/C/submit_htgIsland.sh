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
for mig in 0.01 0.1 0.2 0.3 0.4 0.5 0.75 0.9 #0.01 0.025 0.05 0.075 0.1 0.125 0.15 0.2 0.25 0.3 0.35 0.4 0.45 0.5
do
for omega in 0.05 0.1 0.5
do
for ishtg in 0 1
# Change parameters
#  Change the mp parameter
sed -e "s/XXXX/${mp}/" -e "s/NREP 10000000/NREP 1000000/" -e "s/BBBB/${mB}/" -e "s/MMMM/${mutt}/" -e "s/GGGG/${mig}/" -e "s/OOOO/${omega}/"  -e "s/DDDD/${death}/" -e "s/HTG 1/HTG ${ishtg}/" htg-island_base.c > Scripts/Htgisland_${mig}_${mB}_${mp}_${mutt}_${omega}_htg${ishtg}.c

# Compile the script
cc Scripts/Htgisland_${mig}_${mB}_${mp}_${mutt}_${omega}_htg${ishtg}.c -o Scripts/Htgisland_${mig}_${mB}_${mp}_${mutt}_${omega}_htg${ishtg} -lm

# Create the execution file (two lines)
echo -e "#!/bin/bash\n./${THEPATH}Scripts/Htgisland_${mig}_${mB}_${mp}_${mutt}_${omega}_htg${ishtg} > ${THEPATH}Results/Htgisland_${mig}_${mB}_${mp}_${mutt}_${omega}_htg${ishtg}.txt" > Scripts/Htgisland_${mig}_${mB}_${mp}_${mutt}_${omega}_htg${ishtg}.sh

# Make the execution files executable
chmod +x Scripts/Htgisland_${mig}_${mB}_${mp}_${mutt}_${omega}_htg${ishtg}.sh

# Submit the job (-q long.q)
qsub -q long.q Scripts/Htgisland_${mig}_${mB}_${mp}_${mutt}_${omega}_htg${ishtg}.sh

#---------------------------------------------------------------------------------------------------------
done # htg
done # omega
done # mig
done # mB
done # mp
done # mutt
