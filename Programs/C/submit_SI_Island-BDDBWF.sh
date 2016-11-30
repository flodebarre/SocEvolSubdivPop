#! /bin/bash
### NOT USED FOR THE MOMENT
### Need to uptate:
### THEPATH,
### Names of the folders (remove Sigma),
### Add loop on values of Omega

#
# This script submits jobs for the simulations on island-structured populations,
# with a common-good type of E graph.
#

export THEPATH=Sigma/Sigma/

for mutt in 0.01 0.1 0.25
do
for mB in 6.0
do
for mp in 0.5 0.3
do
for mig in 0.01 0.05 0.1 0.2 0.5
do

# BD DB WF ---------------------------------------------------------------------------------------------------
for updating in DB BD WF
do

# Change parameters
#  Change the noselfinteraction parameter (-> common good)
sed "s/noselfinteraction 1/noselfinteraction 0/" islandbase.c > SigmaScripts/temp0.c
#  Change the mp parameter
sed "s/XXXX/${mp}/" SigmaScripts/temp0.c > SigmaScripts/temp1.c
#  Change the mB parameter
sed "s/BBBB/${mB}/" SigmaScripts/temp1.c > SigmaScripts/temp2.c
#  Change the mutation parameter
sed "s/MMMM/${mutt}/" SigmaScripts/temp2.c > SigmaScripts/temp3.c
#  Change the migration parameter
sed "s/GGGG/${mig}/" SigmaScripts/temp3.c > SigmaScripts/temp4.c

# Concatenate scripts to obtain a full simulation file
cat SigmaScripts/temp4.c islandonestep${updating}.c > SigmaScripts/iscr${updating}_${mig}_${mB}_${mp}_${mutt}.c

# Compile the script
cc SigmaScripts/iscr${updating}_${mig}_${mB}_${mp}_${mutt}.c -o SigmaScripts/iscr${updating}_${mig}_${mB}_${mp}_${mutt} -lm

# Create the execution file (two lines)
echo -e "#!/bin/bash\n./${THEPATH}SigmaScripts/iscr${updating}_${mig}_${mB}_${mp}_${mutt} > ${THEPATH}SigmaResults/s0${updating}_${mig}_${mB}_${mp}_${mutt}.txt" > SigmaScripts/is${updating}_${mig}_${mB}_${mp}_${mutt}.sh

# Make the execution files executable
chmod +x SigmaScripts/is${updating}_${mig}_${mB}_${mp}_${mutt}.sh

# Submit the job (-q long.q)
qsub -q long.q SigmaScripts/is${updating}_${mig}_${mB}_${mp}_${mutt}.sh
done # Updating

#---------------------------------------------------------------------------------------------------------
done # mig
done # mB
done # mp
done # mutt
