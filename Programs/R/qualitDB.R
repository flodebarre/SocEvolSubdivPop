rm(list = ls())
#for(i in dev.list())dev.off()

source("globalGraphParms.R")

thed <- 15



mucrit <- function(m, p = 0.45, n = 4, d = thed, b = 15, c = 1){
    ## Arguments:
    #  m   emigration probability
    #  p   mutation bias
    #  n   deme size
    #  d   number of demes
    #  b   benefits of social interactions
    #  c   costs 
  return(
  ((b - c)*(2 + d*(-1 + m))*m + c*d*m*n)/((1 + d*(-1 + m))*(b + c*(-1 + n)))
  )
}

xvals <- seq(0, 1-1/thed, length.out = 501)
xvals <- xvals[-length(xvals)]

mmax <- 1 - 1/thed
mumax <- 0.25
plotpoly <- function(muvals, color, ...){
  clip(0, 1, 0, 1)
  polygon(c(xvals, xvals[1]), c(muvals, muvals[1]), col = color, lwd = 2, ...)
}
fillcol <- "#009999"

# Initialize plot
pdf("Pics/qualitDB.pdf", width = wpdfQ, height = hpdfDB)
par(las = 1, mgp = mgppdf, xpd = FALSE,
    mar = marpdfQ)
plot(c(0,1), c(0,1), type = "n",
     ylim = c(0, mumax), xlim = c(0,mmax),  
     axes = FALSE, ylab = "", xlab = "", cex = cexplot, cex.axis = cexaxis)
mtext(side = 1, text = "Emigration probability m", line = 1.5, cex = cexlab)
mtext(side = 2, text = expression("Mutation probability "*mu), line = 1.55, las = 0, cex = cexlab)
clip(0, 1, 0, 1)
plotpoly(mucrit(xvals, d = 10^6), MakeTransparent(fillcol, 70), lty = 3)
plotpoly(mucrit(xvals), fillcol)
#lines(xvals, mucrit(xvals), lwd = 2)
#lines(xvals, mucrit(xvals, d = 10^6), lty = 3, lwd = 2)
axis(1, pos = 0, cex.axis = cexaxis)
axis(2, pos = 0, cex.axis = cexaxis)
text(x = 0.63, y = 0.17, labels = expression(N[D] %->% infinity), cex = cexlab)
text(x = 0.47, y = 0.117, labels = expression(N[D] == 15), cex = cexlab)
text(x = 0.3, y = c(0.035, 0.22), labels = c(expression(E*"["*bar(X)*"]" >nu), expression(E*"["*bar(X)*"]" <nu)), col = c(gray(1), gray(0.7)), cex = cexlab, frame = TRUE)


dev.off()

system("xdg-open Pics/qualitDB.pdf")
