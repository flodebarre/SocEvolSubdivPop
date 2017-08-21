rm(list = ls())
for(i in dev.list())dev.off()
source("../Mathematica/analyticsQ.R")
source("ESEB_globalParms.R")
suffix <- "Pics/ESEB_Rplot"

migpoints <- seq(0, 1, length.out = 501)
npts <- length(migpoints)
muts <- c(0.001, 0.01, 0.1)
thed <- 15 # Number of demes in the population 

colmut <- rev(mygradient4[2:4])
ltys <- c(1, 2)
lwdfig <- c(3.5, 2)

cexplot <- 1.3
cexlab <- 1.4
cexaxis <- 1
dopdf <- TRUE

wpdfQ <- 6
hpdfQ <- 4.25
marpdfQ <- c(3, 3, 0.5, 0)+0.2

initplotQ <- function(savepdf = FALSE, LC = ""){
  if(savepdf){
    filename <- paste0(suffix, LC, ".pdf")
    pdf(filename, width = wpdfQ, height = hpdfQ)
  }
  par(las = 1, fg = fgcol, bg = bgcol, mar = marpdfQ, cex = cexplot, cex.axis <- cexaxis)
  par(mgp=mgppdf) # Position of the tick labels
  
  plot(0, type = "n", ylim = c(0, 1), xlim = c(0, 1), axes = FALSE, ylab = "", xlab = "")
  axis(1, pos = 0)
  axis(2, pos = 0)
  mtext("Emigration probability m", 1, line = 1.5, cex = cexlab)
  mtext("Relatedness R", 2, line = 1.5, las = 3, cex = cexlab)
}
closeplotQ <- function(savepdf = FALSE, LC = ""){
  if(savepdf){
    dev.off()
    system(paste0("xdg-open ", suffix, LC, ".pdf"))
    }
}

adjy <- c(0, 1, 3)
legtxt <- rep(0, 3)

plotlinesQ <- function(LC){
  yI <- yO <- as.list(muts)
  for (i in seq_along(muts)){
    yI[[i]][1:npts] <- get(paste0("Qin", LC))(p = 0.45, sel = 0, mut = muts[i], g = 0, n = 4, d = thed, Idself = 1, Ieself = 0, m = migpoints)
    yO[[i]][1:npts] <- get(paste0("Qout", LC))(p = 0.45, sel = 0, mut = muts[i], g = 0, n = 4, d = thed, Idself = 1, Ieself = 0, m = migpoints)
    lines(migpoints, (yI[[i]] - yO[[i]])/(1 - yO[[i]]), type = "l", col = colmut[i], lty = ltys[1], lwd = lwdfig[1])
  }
  midpt <- floor(npts/2.5)
  deltaadj <- 0.5
  xleg <- 0.5
  yleg <- 0.9
  legend(xleg, yleg, col = colmut, legend = muts, lty = rep(1, 3), lwd = rep(lwdfig[1], 3), bty = "n", cex = 0.9)
  text(xleg, yleg, adj = c(-2, 0.5), labels = expression(mu * " ="))
}

for(LC in c("M", "WF")){
  initplotQ(dopdf, LC = LC)
  plotlinesQ(LC)
  closeplotQ(dopdf, LC = LC)
}


