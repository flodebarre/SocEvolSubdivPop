rm(list = ls())
for(i in dev.list())dev.off()

source("globalGraphParms.R")

source("../Mathematica/analytics.R")

specify_decimal <- function(x, k) format(round(x, k), nsmall=k)

## Load the data
thed <- 15 # Number of demes in the population
source("loadData.R")

alldata[is.na(alldata$pA),] # Check if failed simulations

plotDist <- function(i){
  v <- unname(unlist(res[[i]])) # Extract the vector of results
  nn <- length(v)-1 # Max population size
  nAs <- seq(0, nn)/nn # Vector of abundances
  plot(nAs, v)
}


################################################################
## PLOTTING

  # PROPORTION OF ALTRUISTS IN THE POPULATION
  # nA / (nA + nB)
PlotProp <- function(upd, sel, htg, ylim=c(0,1), addAnalysis=FALSE, pdf = TRUE, addTitle = FALSE, plotData = TRUE, muL = mutList, migL = migList, ids = 1, ies = 0, addMu0 = FALSE, theg = 0, sameDE = FALSE){
  
  # upd Update rule ("BD", "DB", "WF")
  # sel value of delta, selection strength
  # htg whether deme sizes are unequal
  # ylim range of the y axis
  # addAnalysis whether to add analytical curves
  # pdf whether to save as pdf
  # addTitle whether to add a plot title
  # plotData whether to plot simulation data (if exists)
  # muL vector of mutation values
  # migL vector of migration values
  # ids Idself, whether self-replacement (d_{ii})
  # ies Ieself, whether interactions with oneself (e_{ii})
  # addMu0 whether to add a curve corresponding to mu -> 0
  # theg value of g, interaction graph equivalent of m (prop interactions outside deme)
  # sameDE whether the D and E graphs are to be the same
  
    # Check consistency of the input
    if(sameDE){
      if(theg != 0) stop("You cannot specify g if you want D and E to be the same!")
      if(ids != ies) stop("Idself and Ieself have to be equal if you want D and E to be the same!")
    }

    # Initializations if not pdf
    thecex <- cexpoints <- cexlab <- 1
    if(pdf){
      thecex <- 1.2 # Global cex value
      cexpoints <- 1.6 # cex of the points
      cexlab <- 1.6 # cex of the labels
      
      DEindic <- ext <- ""
      if(ids!=1) ext <- "_nodself"
      if(sameDE) DEindic <- "_sameDE"
      filename <- paste0('Pics/', "EX", upd, "_sel", sel, "_htg", htg, ext, DEindic) # Name of the pdf file

      pdf(paste0(filename, ".pdf"), width = 4., height = 5., compress = FALSE) # Open pdf
      if(addTitle) martit <- 2.5 else martit <- 0
      par(mar = c(2.5, 2.5, martit, 0)+0.1) # Margins
      par(mgp=c(3, .6, 0)) # Position of the tick labels
      par(cex=thecex, cex.lab=0.5*thecex) # Point sizes
    }
    par(bg=bgcol, fg=fgcol, col=fgcol, 
        col.axis=fgcol, col.lab=fgcol, 
        col.main=fgcol, col.sub=fgcol) # background and foreground colors
    
    par(las = 1) # Orientation of labels on axes

    # Initialize the plot window
    plot(0, type = "n", xlim = c(0,max(migL)), ylim = ylim, 
         xlab = "", ylab = "", axes = FALSE, frame.plot = FALSE)
    if (addTitle) title(main = paste0("sel=", sel, ", htg=", htg))
    colMut <- get(paste0("mygradient", length(muL)))
    
    # Background of the plot region
    rect(0, ylim[1], max(migL), 1, col = rectColor, border = NA)
    # Add horizontal lines
    for(i in seq(0,1,by=0.1)) points(c(0, max(migL)), rep(i,2), col=rectLines, lty=rectLlty, type="l")

    for(imu in  seq_along(muL)){
      if(addAnalysis){ # Add analytical prediction
        # Define a function of mig for the specific set of parameters
        # Definition depends on sameDE (if TRUE, g changes as m)
        if(!sameDE){
      tmpP <- function(x) get(paste0("p", upd))(b=mBList[1], c=1, p=p, sel=sel, mut=muL[imu], m=x, g=theg, n=4, d=thed, Idself=ids, Ieself=ies)
        }else{
      tmpP <- function(x) get(paste0("p", upd))(b=mBList[1], c=1, p=p, sel=sel, mut=muL[imu], m=x, g=x, n=4, d=thed, Idself=ids, Ieself=ies)
        }

        # Plot it
        curve(tmpP, from=0, to=0.9,#par("usr")[2], 
              col=colMut[imu], add = TRUE, lwd = 3)
      }
      if(plotData){
        # Simulation Data: extract the relevant data for the set of parameters
        subdata <- alldata[alldata$mu == muL[imu] & alldata$upd == upd,]
        sub1 <- subdata[subdata$sel== sel & subdata$htg==htg, ]
        for(imig in seq_along(migL)){
          sub <- sub1[sub1$mig == migL[imig], ]
          # Plot estimated frequency
          points(sub$mig, sub$pA, col = colMut[imu], type="p", pch=pchs[imu], cex = cexpoints, bg=MakeTransparent(colMut[imu]))
          # Plot CI
          arrows(sub$mig, sub$pA - sub$dci, sub$mig, sub$pA + sub$dci, 
                 col = colMut[imu], angle = 90, length = 0.075, code = 3)
        }
      }
    }
    if(addMu0){
      # Add mu = 0
      epsmu <- 0.00000001
      if(!sameDE){
        tmpP <- function(x) get(paste0("p", upd))(b=mBList[1], c=1, p=p, sel=sel, mut=epsmu, m=x, g=0, n=4, d=thed, Idself=ids, Ieself=ies)
      }else{
        tmpP <- function(x) get(paste0("p", upd))(b=mBList[1], c=1, p=p, sel=sel, mut=epsmu, m=x, g=x, n=4, d=thed, Idself=ids, Ieself=ies)
      }
      # Plot it
      curve(tmpP, from=0.001, to=0.9,#par("usr")[2], 
            col='blue', add = TRUE, lwd = 2.5, lty = 2)
    }
    # Add horizontal line for the non-selection value
    points(c(0, max(migL)), rep(p,2), col="black", lty=2, type="l", lwd=1.2)

    # Add axes
    axis(1, pos = ylim[1], col=fgcol)
    atseq <- seq(0, 1, by = 0.1)
    axis(2, pos = 0, col=fgcol, at = atseq, labels = atseq, cex.axis = 1)
    axis(2, pos = 0, col=fgcol, at = p, labels = expression(italic(nu)), cex.axis = 1.5)
    mtext(side = 2, "Proportion of altruists", line = 1.5, las=0, cex=cexlab)
    mtext(side = 1, expression(paste("Emigration probability (",italic(m),")")), line = 1.5, las=0, cex=cexlab)
    if(pdf){
      dev.off()
      #system(paste0("xdg-open ", paste0(filename, ".pdf")))
    }
}

themutList <- rev(mutList)#c(0.005, 0.010, 0.100, 0.250)
themigList <- migList #c(0.01, 0.05, 0.10, 0.15, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90)
dylm <- 0.2
ylm <- p + c(-dylm, dylm)

# FIGURE 2
PlotProp("DB", 0.005, 0, addAnalysis = TRUE, ylim = ylm, muL = themutList, migL = themigList, addMu0 = TRUE)

PlotProp("BD", 0.005, 0, addAnalysis = TRUE, ylim = ylm, muL = themutList, migL = themigList, addMu0 = TRUE)

PlotProp("WF", 0.005, 0, addAnalysis = TRUE, ylim = ylm, muL = themutList, migL = themigList, addMu0 = TRUE)

# FIGURE S1
PlotProp("DB", 0.1, 0, addAnalysis = FALSE, muL = themutList, migL = themigList)
PlotProp("BD", 0.1, 0, addAnalysis = FALSE, muL = themutList, migL = themigList)
PlotProp("WF", 0.1, 0, muL = themutList, migL = themigList)

# FIGURE S2
PlotProp("DB", 0.005, 1, muL = themutList, migL = themigList, ylim = ylm)
PlotProp("BD", 0.005, 1, muL = themutList, migL = themigList, ylim = ylm)
PlotProp("WF", 0.005, 1, muL = themutList, migL = themigList, ylim = ylm)

# FIGURE S3
PlotProp("DB", 0.005, 0, muL = themutList, migL = themigList, ids = 0, plotData = FALSE, addAnalysis = TRUE, ylim = ylm, addMu0 = TRUE)
PlotProp("BD", 0.005, 0, muL = themutList, migL = themigList, ids = 0, plotData = FALSE, addAnalysis = TRUE, ylim = ylm, addMu0 = TRUE)
PlotProp("WF", 0.005, 0, muL = themutList, migL = themigList, ids = 0, plotData = FALSE, addAnalysis = TRUE, ylim = ylm, addMu0 = TRUE)

# FIGURE S4
PlotProp("DB", 0.005, 0, muL = themutList, migL = themigList, ids = 0, plotData = FALSE, addAnalysis = TRUE, ylim = ylm, addMu0 = TRUE, sameDE = TRUE)
PlotProp("BD", 0.005, 0, muL = themutList, migL = themigList, ids = 0, plotData = FALSE, addAnalysis = TRUE, ylim = ylm, addMu0 = TRUE, sameDE = TRUE)
PlotProp("WF", 0.005, 0, muL = themutList, migL = themigList, ids = 0, plotData = FALSE, addAnalysis = TRUE, ylim = ylm, addMu0 = TRUE, sameDE = TRUE)

