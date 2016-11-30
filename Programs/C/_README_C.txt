<!---
README file for the C/ folder

F. Débarre 2016
-->

# Contents

- `.c` scripts
- `.sh` code to execute the scripts on a cluster
- `Scripts/` folder, where the concatenated scripts and executables are stored
- `Results/` folder, where the simulation results (`.txt`) are stored.

# To execute

The simulations were run on the Migale cluster (http://migale.jouy.inra.fr), which has a `qsub` type queuing system.
To execute the scripts:

- Upload the entire C folder to the cluster
- Update the value of `THEPATH` in the two `submit_*.sh` files,
  so that the path matches the path to the scripts in your system.
- Type
    ./submit_all.sh
   to submit all the jobs for all simulations to the cluster.
  If you want to do it step by step, type
    ./submit_G12-BDDBWF.sh
  for the graph-structured population, or
    ./submit_Island-BDDBWF.sh (or _SI_)
  for the island model, in order to submit the jobs to the cluster.
- The results (`.txt` files) are stored in the `SigmaResults/` folder.

# `.c` scripts =

The `.c` scripts are presented into parts that are then combined into `.c` scripts stored in the `SigmaScripts/` folder.

Prefixes:
 - `graph_` : graph-structured populations,
 - `island_`: island-structured populations.

Suffixes:
 - `_base.c`: base file, common to all updating rules,
              to which updating-rule specific code will be appended.
 - `_onestepUR.c` : code specific to each updating rule, where
                    UR is the two-letter name of the updating rule (WF, BD, DB).

For the graph-structured populations, we also have scripts named
   `graph12_*.c`, which contain the code for the graph
    (F: Frucht, W: wheel-like, H: hexagon-like).
