rm(list = ls())
#for(i in dev.list())dev.off()

source("globalGraphParms.R")
Power <- function(a, b) return(a^b)

thed <- 15
theb <- 15
then <- 4

colmu0 <- colnomut
colmu1 <- "#7F557F"
colmu2 <- "#FFAA00"
mus <- c(0.05, 0.2)

CBrDB <- function(b = theb, c = 1, m, n = then, d = thed){
  return(
  -(((b - c)*(-1 + d*Power(-1 + m,2) + 2*m) + c*(-1 + d)*n)/(-(c*(-1 + d*Power(-1 + m,2) + 2*m)*(-1 + n)) + b*(1 - d*Power(-1 + m,2) + 2*m*(-1 + n) + d*(-2 + m)*m*n)))
  )
}

RM <- function(m, mut, n = then, d = thed){
  -(((1 + d*(-1 + m))*(-1 + mut))/(1 + mut*(-1 + n) + d*(-1 + mut + m*(-1 + mut)*(-1 + n) - mut*n)))
}

mmin <- 0.0005
#xvals <- exp(seq(log(mmin), log(1-1/thed), length.out = 501))
xvals <- seq(mmin, 1-1/thed, length.out = 501)
thelwd <- 3
lwdBC <- 5
BCcol <- "#000000"

pdf("Pics/explainDB.pdf", width = wpdfQ, height = hpdfDB)
par(las = 1, mgp = mgppdf, xpd = FALSE,
    mar = marpdfQ)

plot(xvals, CBrDB(m = xvals), type = "l", col = BCcol, log = "", 
     axes = FALSE, ylim = c(0,1),
     ylab = "", xlab = "", lwd = lwdBC, cex.axis = cexaxis)
for(imu in seq_along(mus)){
  lines(xvals, RM(m = xvals, mut = mus[imu]), lty = 1, col = get(paste0("colmu", imu)), lwd = thelwd)
}
lines(xvals, RM(m = xvals, mut = 0), lty = 2, col = colnomut, lwd = thelwd)
lines(c(0, 1-1/thed), rep(0, 2))
axis(1, pos = 0, cex.axis = cexaxis)
axis(2, pos = mmin, at = seq(0, 1, by = 0.2), cex.axis = cexaxis)
mtext(side = 1, text = "Emigration probability m", line = 1.5, cex = cexlab)
mtext(side = 2, text = expression("Relatedness R and C/B ratio"), line = 1.55, las = 0, cex = cexlab)

xleg <- 0.4
yleg <- 1
legend(xleg, yleg, col = c(col0, colmu1, colmu2, BCcol), 
       legend = c(expression("R("* mu == 0*")"), 
                  expression("R("* mu == 0.05*")"), 
                  expression("R("* mu == 0.2*")"), 
                  "C/B"), 
       lty = c(2, 1, 1, 1), lwd = c(rep(thelwd, 3), lwdBC), bty = "n", cex = cexaxis,
       seg.len = 2)


dev.off()

system("xdg-open Pics/explainDB.pdf")

###############################################################


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
mtext(side = 2, text = expression("Mutation probability "*mu), line = 1.65, las = 0, cex = cexlab)
clip(0, 1, 0, 1)
plotpoly(mucrit(xvals, d = 10^6), MakeTransparent(fillcol, 70), lty = 1, lwd = 0.5)
plotpoly(mucrit(xvals), fillcol)
#lines(xvals, mucrit(xvals), lwd = 2)
#lines(xvals, mucrit(xvals, d = 10^6), lty = 3, lwd = 2)

axis(1, pos = 0, cex.axis = cexaxis)
axis(2, pos = 0, cex.axis = cexaxis)

lwdmu <- 2.5
for(i in 1:2){
  lines(c(0, mmax), rep(mus[i], 2), lty = 3, col = get(paste0("colmu", i)), lwd = lwdmu)
}
lines(c(0, mmax), rep(0, 2), lty = 3, col = colmu0, lwd = lwdmu)

text(x = 0.63, y = 0.17, labels = expression(N[D] %->% infinity), cex = cexplot)
text(x = 0.47, y = 0.117, labels = expression(N[D] == 15), cex = cexplot)
text(x = 0.3, y = c(0.03, 0.22), labels = c(expression(E*"["*bar(X)*"]" >nu), expression(E*"["*bar(X)*"]" <nu)), col = c(gray(1), gray(0.7)), cex = cexplot)


dev.off()

system("xdg-open Pics/qualitDB.pdf")