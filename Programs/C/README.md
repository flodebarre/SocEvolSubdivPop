<!---
README file for the C/ folder
-->

# Contents

- `.c` scripts,
- `.sh` code to execute the scripts on a cluster,
- `Scripts/` folder, where the concatenated scripts and executables are stored,
- `Results/` folder, where the simulation results (`.txt`) are stored,

But also folders containing scripts not used in the paper but kept for the record:

- `Oldies/`     folder, old versions of the script (can only do homogeneous population), experimental scripts (not to be shared on Dryad).

# To execute

The simulations were run on the Migale cluster <http://migale.jouy.inra.fr>, which has a `qsub` type queuing system.

To execute the scripts:

- Upload the entire `C/` folder to the cluster,
- Update the value of `THEPATH` in the two `submit_*.sh` files,
  so that the path matches the path to the scripts in your system.
- Type
```
    ./submit_all.sh
```
   to submit all the jobs for all simulations to the cluster.
- The results (`.txt` files) are stored in the `Results/` folder.

# `.c` scripts =

The simulation scripts are split into different parts.
The `submit*.sh` program combines these different sub-scripts into new scripts stored in the `Scripts/` folder.

- `_base.c` contains the part common to all programs.
- `onestep*.c` is the specific part for each life-cycle (WF, BD, DB).
