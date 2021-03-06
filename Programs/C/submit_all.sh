#! /bin/bash

#
# This script submits jobs for the simulations on island-structured populations.
#

export THEPATH=SocEvolSubdivPop/

for mutt in 0.001 0.01 0.1 0.25
do
for mB in 15.0
do
for mp in 0.45
do
for mig in 0.01 0.025 0.05 0.075 0.1 0.125 0.15 0.2 0.25 0.3 0.35 0.4 0.45 0.5 0.6 0.7 0.8 0.9
do
for omega in 0.005 0.1
do
for ishtg in 0 1
do

# BD DB WF ---------------------------------------------------------------------------------------------------
for updating in DB BD WF
do
# Change parameters
#  Change the mp parameter
sed -e "s/XXXX/${mp}/" -e "s/NREP 10000000/NREP 1000000/" -e "s/BBBB/${mB}/" -e "s/MMMM/${mutt}/" -e "s/GGGG/${mig}/" -e "s/OOOO/${omega}/"  -e "s/DDDD/${death}/" -e "s/HTG 1/HTG ${ishtg}/" htg-island_base.c >  Scripts/tmp.c

# Concatenate scripts to obtain a full simulation file
cat Scripts/tmp.c htg-island_onestep${updating}.c > Scripts/HtgIsl${updating}_${mig}_${mB}_${mp}_${mutt}_${omega}_htg${ishtg}.c

# Compile the script
cc Scripts/HtgIsl${updating}_${mig}_${mB}_${mp}_${mutt}_${omega}_htg${ishtg}.c -o Scripts/HtgIsl${updating}_${mig}_${mB}_${mp}_${mutt}_${omega}_htg${ishtg} -lm

# Create the execution file (two lines)
echo -e "#!/bin/bash\n./${THEPATH}Scripts/HtgIsl${updating}_${mig}_${mB}_${mp}_${mutt}_${omega}_htg${ishtg} > ${THEPATH}Results/HtgIsl${updating}_${mig}_${mB}_${mp}_${mutt}_${omega}_htg${ishtg}.txt" > Scripts/HtgIsl${updating}_${mig}_${mB}_${mp}_${mutt}_${omega}_htg${ishtg}.sh

# Make the execution files executable
chmod +x Scripts/HtgIsl${updating}_${mig}_${mB}_${mp}_${mutt}_${omega}_htg${ishtg}.sh

# Submit the job (-q long.q)
qsub -q long.q Scripts/HtgIsl${updating}_${mig}_${mB}_${mp}_${mutt}_${omega}_htg${ishtg}.sh

done
#---------------------------------------------------------------------------------------------------------
done # htg
done # omega
done # mig
done # mB
done # mp
done # mutt
