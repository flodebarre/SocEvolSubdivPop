# Clean memory
rm(list = ls())

# Load analytical expressions exported from Mathematica
source("../../Programs/Mathematica/analyticsQ.R")
# Global parameters
source("Talk_globalGraphParms.R")

# File names of the figures
prefix <- "../Pics/Rplot"

# Mutation values used in the figure
muts <- c(0.0000000001, 0.001, 0.01, 0.1, 0.25)
thed <- 15 # Number of demes in the population 
migpoints <- seq(0, 1-1/thed, length.out = 501) # x values (emigration probabilities)
npts <- length(migpoints)

# Colors of the lines
colmut <- c(colnomut, rev(mygradient4[(5-length(muts)):4]))
# Line types
ltys <- c(2, rep(1, length(muts)-1))
# Line widths
lwdfig <- c(3.5, 2)

# Initialize plot
initplotR <- function(savepdf = FALSE, LC = "", ext = ""){
  if(savepdf){
    filename <- paste0(prefix, LC, ext, ".pdf")
    pdf(filename, width = wpdfQ, height = hpdfQ)
  }
  par(las = 1, fg = fgcol, bg = bgcol, mar = marpdfQ, cex = cexplot, cex.axis <- cexaxis)
  par(mgp=mgppdf) # Position of the tick labels
  
  plot(0, type = "n", ylim = c(0, 1), xlim = c(0, max(migpoints)), axes = FALSE, ylab = "", xlab = "")
  axis(1, pos = 0)
  axis(2, pos = 0)
  mtext("Emigration probability m", 1, line = 1.5, cex = cexlab)
  mtext("Relatedness R", 2, line = 1.5, las = 3, cex = cexlab)
}

# Close plot
closeplotR <- function(savepdf = FALSE, LC = "", ext = ""){
  if(savepdf){
    dev.off()
    system(paste0("xdg-open ", prefix, LC, ext, ".pdf"))
    }
}

# Plot the R curves
plotlinesR <- function(LC, seqi = rev(seq_along(muts))){
  yI <- yO <- as.list(muts) # Initialize lists for Qin and Qout
  for (i in seqi){
    # Load functions exported from Mathematica, with our parameters
    yI[[i]][1:npts] <- get(paste0("Qin", LC))(p = 0.45, sel = 0, mut = muts[i], g = 0, n = 4, d = thed, Idself = 1, Ieself = 0, m = migpoints)
    yO[[i]][1:npts] <- get(paste0("Qout", LC))(p = 0.45, sel = 0, mut = muts[i], g = 0, n = 4, d = thed, Idself = 1, Ieself = 0, m = migpoints)
    # Plot R=(Qin-Qout)/(1-Qout)
    lines(migpoints, (yI[[i]] - yO[[i]])/(1 - yO[[i]]), type = "l", col = colmut[i], lty = ltys[i], lwd = lwdfig[1])
  }
  # Add legend
  xleg <- 0.5 # x position
  yleg <- 0.9 # y position
  legend(xleg, yleg, col = colmut, legend = round(muts[seqi], 5), lty = ltys[seqi], lwd = rep(lwdfig[1], 3), bty = "n", cex = 1)
  # Add title to the legend box
  text(xleg, yleg, adj = c(-2, 0.5), labels = expression(mu * " ="))
}

seqs <- list(c(1), c(1,2), c(1,2,3), c(1,2,3,4), c(1,2,3,4,5))
# Plot for each life-cycle
for(LC in c("M", "WF")){
 for(j in seq_along(seqs)){
    initplotR(dopdf, LC = LC, ext = j)
    plotlinesR(LC, seqi = seqs[[j]])
    closeplotR(dopdf, LC = LC, ext = j)
 }
}


