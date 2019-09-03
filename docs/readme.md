There are scripts in `C`, `Mathematica` and `R`. You can download all the scripts as a zipped file here: [click to download](https://github.com/flodebarre/SocEvolSubdivPop/archive/master.zip).

# C
Stochastic simulations:

## Folders
 - `Scripts/` will store the combined scripts;
 - `Results/` will store the outputs of the simulations.

## `C` files
They are combined to form a single file, depending on the life-cycle
 -  `htg-island_base.c` [(click here to download)](https://github.com/flodebarre/SocEvolSubdivPop/raw/master/Programs/C/htg-island_base.c)
 -  `htg-island_onestepBD.c` [(click here to download)](https://github.com/flodebarre/SocEvolSubdivPop/raw/master/Programs/C/htg-island_onestepBD.c)
 -  `htg-island_onestepDB.c` [(click here to download)](https://github.com/flodebarre/SocEvolSubdivPop/raw/master/Programs/C/htg-island_onestepDB.c)
 -  `htg-island_onestepWF.c` [(click here to download)](https://github.com/flodebarre/SocEvolSubdivPop/raw/master/Programs/C/htg-island_onestepWF.c)

## Submission file
The simulations were run on the [Migale cluster](http://migale.jouy.inra.fr), which has a `qsub` type queuing system.

To execute the scripts:

 -  Upload the entire C/ folder to the cluster,
 -  Update the value of THEPATH in the `submit_*.sh` file, so that the path matches the path to the scripts in your system.
Type
```
    ./submit_all.sh
```
to submit all the jobs for all simulations to the cluster. The `submit_all.sh` file is available [(here)](https://github.com/flodebarre/SocEvolSubdivPop/raw/master/Programs/C/submit_all.sh)

The results (`.txt` files) are stored in a `Results/` folder.

---


# Mathematica
Analytical part:
 - `.pdf` output of the Mathematica notebook [(click here to download)](https://github.com/flodebarre/SocEvolSubdivPop/raw/master/Programs/Mathematica/Debarre_2019_DGAA_SI.pdf)
 - `.nb` source file (same content as pdf version, but editable) [(click here to download)](https://github.com/flodebarre/SocEvolSubdivPop/raw/master/Programs/Mathematica/Debarre_2019_DGAA_SI.nb)

---


# R
To plot the figures, using simulation data and outputs from Mathematica.

   - `AllFigs.R` is a script to plot all Figures at once [(click here to download)](https://github.com/flodebarre/SocEvolSubdivPop/raw/master/Programs/R/AllFigs.R),
   - `globalGraphParms.R` contains graphical parameters (e.g. colors) [(click here to download)](https://github.com/flodebarre/SocEvolSubdivPop/raw/master/Programs/R/globalGraphParms.R),
   - `loadData.R` is a script to... load the simulation data [(click here to download)](https://github.com/flodebarre/SocEvolSubdivPop/raw/master/Programs/R/loadData.R),
   - `Rplots.R` plots the panels of Figure 1 [(click here to download)](https://github.com/flodebarre/SocEvolSubdivPop/raw/master/Programs/R/Rplots.R),
   - `EXplots.R` plots the panels of Figure 2, and supplementary Figures S1-S4 [(click here to download)](https://github.com/flodebarre/SocEvolSubdivPop/raw/master/Programs/R/EXplots.R).
   - `explainDB.R` plots the panels of Figure 3 [(click here to download)](https://github.com/flodebarre/SocEvolSubdivPop/raw/master/Programs/R/explainDB.R),
   - `Qplots.R` plots the panels of Figure S5 [(click here to download)](https://github.com/flodebarre/SocEvolSubdivPop/raw/master/Programs/R/Qplots.R),

You also need a `Pics/` folder, that will contain the pdf of the generated figures.

*NB*: There are commands to open the newly created figures (`system("xdg-open...")`). This only works with Unix. With a Mac, you need to replace `xdg-open` by `open`. On Windows... I don't know! But it does not matter, you can always open the files manually!
