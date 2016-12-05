#! /bin/bash

#
# This script submits jobs for the simulations on island-structured populations,
# with a no-self interactions type of E graph.
#

export THEPATH=SocEvolSubdivPop/

for mutt in 0.01 0.1 0.25
do
for mB in 15.0
do
for mp in 0.45
do
for mig in 0.01 0.05 0.1 0.2 0.35 0.5
do
for omega in 0.005 0.05 0.5
do

# BD DB WF ---------------------------------------------------------------------------------------------------
for updating in DB BD WF
do

# Change parameters
#  Change the mp parameter
sed "s/XXXX/${mp}/" island_base.c > Scripts/temp1.c
#  Change the mB parameter
sed "s/BBBB/${mB}/" Scripts/temp1.c > Scripts/temp2.c
#  Change the mutation parameter
sed "s/MMMM/${mutt}/" Scripts/temp2.c > Scripts/temp3.c
#  Change the migration parameter
sed "s/GGGG/${mig}/" Scripts/temp3.c > Scripts/temp4.c
#  Change the Strength of selection parameter
sed "s/OOOO/${omega}/" Scripts/temp4.c > Scripts/temp5.c
#  Change the number of demes
sed "s/NDEMES 4/NDEMES 30/" Scripts/temp5.c > Scripts/temp6.c

# Concatenate scripts to obtain a full simulation file
cat Scripts/temp6.c island_onestep${updating}.c > Scripts/Biscr${updating}_${mig}_${mB}_${mp}_${mutt}_${omega}.c

# Compile the script
cc Scripts/Biscr${updating}_${mig}_${mB}_${mp}_${mutt}_${omega}.c -o Scripts/Biscr${updating}_${mig}_${mB}_${mp}_${mutt}_${omega} -lm

# Create the execution file (two lines)
echo -e "#!/bin/bash\n./${THEPATH}Scripts/Biscr${updating}_${mig}_${mB}_${mp}_${mutt}_${omega} > ${THEPATH}Results/Bs1${updating}_${mig}_${mB}_${mp}_${mutt}_${omega}.txt" > Scripts/Bis${updating}_${mig}_${mB}_${mp}_${mutt}_${omega}.sh

# Make the execution files executable
chmod +x Scripts/Bis${updating}_${mig}_${mB}_${mp}_${mutt}_${omega}.sh

# Submit the job (-q long.q)
qsub -q long.q Scripts/Bis${updating}_${mig}_${mB}_${mp}_${mutt}_${omega}.sh
done # Updating

#---------------------------------------------------------------------------------------------------------
done # omega
done # mig
done # mB
done # mp
done # mutt
