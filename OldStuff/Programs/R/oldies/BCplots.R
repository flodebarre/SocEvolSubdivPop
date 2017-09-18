rm(list = ls())
for(i in dev.list())dev.off()

source("../Mathematica/analyticsBC.R")
source("globalGraphParms.R")

migpoints <- seq(0, 1, length.out = 501)
npts <- length(migpoints)
muts <- c(10^(-4), 0.005, 0.1)

colmut <- c("#152D7B", "#234BCD", "#976C52")
ltys <- c(1, 2)
lwdfig <- 2.5

cexplot <- 1.3
cexlab <- 1.4
cexaxis <- 1
dopdf <- TRUE

wpdfQ <- 5
hpdfQ <- 4.25
marpdfQ <- c(3, 3, 0.5, 0)+0.2
initplotBC <- function(savepdf = FALSE, LC = ""){
  if(savepdf){
    filename <- paste0("Pics/BCplot", LC, ".pdf")
    pdf(filename, width = wpdfQ, height = hpdfQ)
  }
  par(las = 1, fg = fgcol, bg = bgcol, mar = marpdfQ, cex = cexplot, cex.axis <- cexaxis)
  par(mgp=mgppdf) # Position of the tick labels
  
  plot(0, type = "n", ylim = c(0, 1), xlim = c(0, 1), axes = FALSE, ylab = "", xlab = "")
  axis(1, pos = 0)
  axis(2, pos = 0)
  mtext("Emigration probability m", 1, line = 1.5, cex = cexlab)
  mtext("Direct and Indirect effects", 2, line = 1.5, las = 3, cex = cexlab)
}
closeplotBC <- function(savepdf = FALSE, LC = ""){
  if(savepdf){
    dev.off()
    system(paste0("xdg-open ", "Pics/BCplot", LC, ".pdf"))
    }
}
plotlinesBC <- function(savepdf = FALSE, LC, mu, color = 2, b=15){
    bD <- get(paste0("b", LC, "D"))(p = 0.45, sel = 0, mut = mu, g = 0, n = 4, d = 30, Idself = 1, Ieself = 0, m = migpoints)
    bI <- get(paste0("b", LC, "I"))(p = 0.45, sel = 0, mut = mu, g = 0, n = 4, d = 30, Idself = 1, Ieself = 0, m = migpoints)
    cD <- get(paste0("c", LC, "D"))(p = 0.45, sel = 0, mut = mu, g = 0, n = 4, d = 30, Idself = 1, Ieself = 0, m = migpoints)
    if(length(cD) == 1) cD <- rep(cD, npts)
    cI <- get(paste0("c", LC, "I"))(p = 0.45, sel = 0, mut = mu, g = 0, n = 4, d = 30, Idself = 1, Ieself = 0, m = migpoints)
    
    # Initiate the plot
    if(savepdf){
      filename <- paste0("Pics/BCplot", LC, ".pdf")
      pdf(filename, width = wpdfQ, height = hpdfQ)
    }
    par(las = 1, fg = fgcol, bg = bgcol, mar = marpdfQ, cex = cexplot, cex.axis <- cexaxis)
    par(mgp=mgppdf) # Position of the tick labels
    
    plot(0, type = "n", ylim = range(c(b*(bD-bI), cD-cI)), xlim = c(0, 1), axes = FALSE, ylab = "", xlab = "")
    axis(1, pos = par("usr")[1])
    axis(2, pos = 0)
    mtext("Emigration probability m", 1, line = 1.5, cex = cexlab)
    mtext("Direct and Indirect effects", 2, line = 1.5, las = 3, cex = cexlab)
    
    
#    lines(migpoints, bD, type = "l", col = color, lty = ltys[1], lwd = 2*lwdfig)
#    lines(migpoints, bI, type = "l", col = color, lty = ltys[2], lwd = 2*lwdfig)
    
#    lines(migpoints, cD, type = "l", col = color, lty = ltys[1], lwd = lwdfig)
#    lines(migpoints, cI, type = "l", col = color, lty = ltys[2], lwd = lwdfig)

    lines(migpoints, cD-cI, type = "l", col = 2, lty = 2, lwd = lwdfig)
    lines(migpoints, b*(bD-bI), type = "l", col = 2, lty = 1, lwd = lwdfig)
#      if(yI[[i]][npts] > 0.95) adjy <- 1.25 else adjy <- -0.3
    
#    text(1, yI[[i]][npts], labels = substitute(mu * "=" * MM, list(MM = muts[i])), adj = c(1, adjy), col = colmut[i])
  
#  midpt <- floor(npts/4)
#  deltaadj <- 0.5
#  text(migpoints[midpt], yI[[2]][midpt], labels = "Qin", adj = c(0, 0 - deltaadj))
#  text(migpoints[midpt], yO[[2]][midpt], labels = "Qout", adj = c(0, 1 + deltaadj))
}

LC <- "BD"
#initplotBC(FALSE, LC = LC)
plotlinesBC(LC = "WF", mu = 0.005)
plotlinesBC(LC = "WF", mu = 0.9)
closeplotBC(FALSE, LC = LC)
plotlinesBC(LC = LC, mu = 0.1)

stop()

#par(mfrow = c(1,2 ))
# Moran
# Initiate plot
for(LC in c("M", "WF")){
  initplotQ(dopdf, LC = LC)
  plotlinesQ(LC)
  closeplotQ(dopdf, LC = LC)
}
