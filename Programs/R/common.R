## Code common to all R scripts

#--------------------------------------------------------------------------
## USER INPUT NEEDED!!

# Where to save the plots
picdir <- ""

#--------------------------------------------------------------------------

# COMPUTER PARAMETES
# Where simulation data are stored
pathtodata <- "../C/Results/" 

## MODEL PARAMETERS
# Strength of selection
omegalist <- c(0.005, 0.05, 0.5) 

# Updating rule
updatelist <- c("WF", "BD", "DB")

# Mutation bias (1/2: no bias)
plist <- c(0.45) 

# Emigration probability
miglist <- sort(c(0.01, 0.05, 0.1, 0.2, 0.5, 0.025, 0.075, 0.15, 0.3, 0.4))

# Mutation probability
mulist <- c(0.01, 0.1, 0.25)

# Interaction (payoff) matrix is
# | b-c  -c |
# |  b    0 |

# Benefit from interation
Blist <- c(15.0)

thec <- 1.0 # Cost (c) parameter


posax2 <- 0 # Position of the axes
mgppdf <- c(1.65, 0.5, 0) # Label position
cexpdf <- 1.3 # Global cex in the pdf

# Function to initialize the plotting window
initplot.mig <- function(ylim=c(0,1), xlim=c(0,1), ylb=expression(paste("E[", bar(X), "]")), yline=1, las.y=1, byaxy=0.01, ...){
  par(las=1)
  plot(0, 0, type="n", axes=FALSE, xlab=expression(paste("Migration (m)")), ylab="", xlim=xlim, ylim=ylim)
  axis(1)
  axis(2, pos=posax2, at=pretty(seq(ylim[1], ylim[2], by=byaxy)), ...)
  mtext(side=2, text=ylb, las=1, line=mgppdf[1], cex=cexpdf, las=las.y)
  segments(0, yline, xlim[2], yline, lty=3)
}

stop("no error")
#--------------------------------------------------------------------------
## THEORETICAL PREDICTIONS
# Define Power function used in the R scripts exported from Mathematica
Power <- function(a,b) a^b
Sqrt <- function(x) sqrt(x)

# Source scripts derived from Mathematica
sourcetheo <- function(pattern=""){
  # List them
  listtheo <- system(paste("ls Rdata/theo*", pattern,"*", sep=""), intern=TRUE); listtheo
  # Source the scripts
  for (i in listtheo) source(i)
}

# Compute sigma from k1 and k2
compute.sigma <- function(k1,k2){
  return(k1/k2)
}

# Compute R from k1 and k2
compute.R <- function(k1,k2){
  return((k1-k2)/(k1+k2))
}

# Compute m from beta and gamma
compute.m <- function(beta, gamma, p, b, c){
  return( p + beta*b - gamma*c)
}


#--------------------------------------------------------------------------
## GRAPHICAL PARAMETERS
# BG FG colors
colbg <- "white"
colfg <- "black"
par(bg=colbg, fg=colfg, col.axis=colfg, col.lab=colfg)
par(las=1)

# COLORS
# Other colors
colC1 <- "#F57900" # Orange tango color
colC2 <- "#8F5902" # Brown tango color

#  Island model
colmigs <- sort(c("#130008", "#71002D", "#AC0045", "#DF1D6C", "#EF5694"), decreasing=TRUE) #c("#da8dac", "#be386e", "#ac0045", "#850035", "#4c001e", "#ecc6d5") # Pink gradient (mutation values)

colmus <- c("#D69FE0", "#A883AF", "#75507b", "#4F3753", "#211723") # Shading from Paletton, using Tango Plum as base color

colBD <- "#8f5902" # Dark Chocolate (Brown)
colDB <- "#fcaf3e" # Orange
colWF <- "#555753" # Aluminium (Gray)

# pch
#  G12 graphs
pch12W <- 10
pch12H <- 5
pch12F <- 0
#  Island model
pchmig <- c(0, 1, 2, 5, 6) # corresponding to mutation values is Island model
pchmut <- c(2, 3, 4, 6, 8) # corresponding to mutation values is Island model

# cex
cex12W <- 1.15
cex12H <- 1.4 # just to make these points bigger so that they are still visible when behind other points
cex12F <- 0.75

# Line widths
lwdtheo <- 2 # Line width of the theoretical prediction
lwdpch <- 1.1 # LWD of the simulation points



# pdf parameters
wpdf <- 3 # Width
hpdf <- 3.2  # Height
marpdf <- c(2.5, 3., 0.05, 0.0) # Margins
mgppdf <- c(1.65, 0.5, 0) # Label position
thefamily <- "Helvetica" # Font family
tclpdf <- -0.3 # Length of ticks
save.as.pdf <- TRUE

murange <- c(10^-6, 0.22) # Range of mu values on the plots

# Function to lighten color
lighten <- function(color, factor=1.4){
  col <- col2rgb(color)*factor
  return(rgb(t(col), maxColorValue=255))
}

# PLOTTING FUNCTIONS----------------------------------------------------------------------
# INITIALIZE PLOTS
# Initialize plot
initplotmu <- function(ylb, yline=1, las.y=1, xlim=murange, ylim=c(0, 1), byaxy=0.01, posax1=NA, ...){
  # ylb: ylab, legend of the vertical axis
  # yline: position of a dotted line
  # ... : anything related to axis2
  par(las=1)
  plot(0, 0, type="n", axes=FALSE, ylab="", xlab=expression(paste("Mutation (", mu, ")")), xlim = xlim, ylim=ylim)
  mtext(side=2, text=ylb, las=las.y, line=mgppdf[1], cex=cexpdf)
  axis(1, pos=posax1)
  axis(2, pos=posax2, at=pretty(seq(ylim[1], ylim[2], by=byaxy)), ...)
  segments(0, yline, xlim[2], yline, lty=3)
}





