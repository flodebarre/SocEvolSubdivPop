for(i in dev.list())dev.off()

source("globalGraphParms.R")


source("../Mathematica/analytics.R")

specify_decimal <- function(x, k) format(round(x, k), nsmall=k)

## Load the data
source("loadData.R")

# 
# # Parameters tested
# mutList <- c(0.001, 0.01, 0.05, 0.1, 0.25)
# migList <- c(0.01, 0.05, 0.1, 0.15, 0.2, 0.21, 0.3, 0.4, 0.5, 0.6, 0.7)
# selList <- c(0.005, 0.1)
# mBList <- c(15.0)
# htgList <- c(0, 1)
# updList <- c("WF", "BD", "DB")
# p <- 0.45
# 
# pars <- expand.grid(mu=mutList, mig=migList, sel=selList, mB = mBList, htg = htgList, upd = updList)
# 
# getwd()
# system("ls *pdf")
# # Load simulation data
# getSimData <- function(mu, mig, sel, mB, htg, upd){
#   fileName <- paste0("../C/Results/HtgIsl", upd,"_",mig,"_", specify_decimal(mB, 1), "_", p, "_",mu,"_",sel, "_htg", htg, ".txt")
#   if(file.exists(fileName)){
#     sz <- file.info(fileName)$size
#     if(sz>1) m <- read.table(fileName) else m <- NA # Remove empty files to avoid errors
#     #if(sum(m[-c(1, length(m))])/sum(m)<0.25) m <- NA # Remove simulations where did not manage to get variation
#   } else {
#     m <- NA
#   }
#   return(m)
# }
# res <- lapply(seq_len(nrow(pars)), function(i) do.call(getSimData, pars[i,]))
# 
# # Note: 
# #  with heterogeneous deme sizes, the total population size may vary!
# 
# computeStats <- function(i){
#   v <- unname(unlist(res[[i]])) # Extract the vector of results
#   popsize <- length(v)-1 # Max population size (-1 because nn+1 elements from 0 to nn)
#   nbAs <- seq(0, popsize) # Vector of abundances
#   nreplicates <- sum(v) # Number of data points
#   m <- sum(v*nbAs)/nreplicates # Compute the mean number of A individuals in the simulations
#   propA <- m/popsize # Convert into proportion of type-A individuals
#   dci <- 1.960 * sqrt(propA * (1-propA) / nreplicates) # Confidence interval for the proportion
#   return(c(pA = propA, dci = dci, popsize = popsize))
# }
# 
# # Compute some stats on the results
# # (mean prop of A, population size)
# simStats <- lapply(seq_len(nrow(pars)), computeStats)
# tmp <- data.frame(matrix(unlist(simStats), byrow = TRUE, ncol = 3))
# names(tmp) <- c("pA", "dci", "popsize")
# 
# # Combine with parameter values
# alldata <- cbind(pars, tmp)
# alldata

alldata[is.na(alldata$pA),]

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
PlotProp <- function(upd, sel, htg, ylim=c(0,1), addAnalysis=FALSE, pdf = TRUE, addTitle = FALSE, plotData = TRUE, muL = mutList, migL = migList, ids = 1, ies = 0){
  
  # upd Update rule ("BD", "DB", "WF")
  # ids Idself, whether self-replacement (d_{ii})
  # ies Ieself, whether interactions with oneself (e_{ii})

    # Initializations if not pdf
    thecex <- cexpoints <- cexlab <- 1
    if(pdf){
      thecex <- 1.2 # Global cex value
      cexpoints <- 1.6 # cex of the points
      cexlab <- 1.6 # cex of the labels
      
      filename <- paste0('Pics/', "EX", upd, "_sel", sel, "_htg", htg) # Name of the pdf file

      pdf(paste0(filename, ".pdf"), width = 4.5, height = 5.5, compress = FALSE) # Open pdf
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

    # Add horizontal line for the non-selection value
    points(c(0, max(migL)), rep(p,2), col="black", lty=2, type="l", lwd=1.2)
    for(imu in  seq_along(muL)){
      if(addAnalysis){ # Add analytical prediction
        # Define a function of mig for the specific set of parameters
      tmpP <- function(x) get(paste0("p", upd))(b=mBList[1], c=1, p=p, sel=sel, mut=muL[imu], m=x, g=0, n=4, d=30, Idself=ids, Ieself=ies)
        # Plot it
        curve(tmpP, from=0, to=0.9,#par("usr")[2], 
              col=colMut[imu], add = TRUE, lwd = 2)
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
    axis(1, pos = ylim[1], col=fgcol)
    atseq <- seq(0, 1, by = 0.1)
    axis(2, pos = 0, col=fgcol, at = c(atseq, p), labels = c(atseq, expression(italic(p))))
    mtext(side = 2, "Proportion of altruists", line = 1.5, las=0, cex=cexlab)
    mtext(side = 1, expression(paste("Emigration probability (",italic(m),")")), line = 1.5, las=0, cex=cexlab)
    if(pdf){
      dev.off()
      system(paste0("xdg-open ", paste0(filename, ".pdf")))
    }
}

selList

for(sel in selList){
  for(upd in updList){
    PlotProp(upd, sel, 1)
    addA <- FALSE
    if(sel<0.01) 
      addA <- TRUE
    PlotProp(upd, sel, 0, addAnalysis = addA)
}
}

themutList <- c(0.005, 0.010, 0.100, 0.250)
themigList <- c(0.01, 0.05, 0.10, 0.15, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90)
dylm <- 0.15
ylm <- p + c(-dylm, dylm)

PlotProp("WF", 0.005, 1, muL = themutList, ylim = ylm)
PlotProp("WF", 0.005, 0, addAnalysis = TRUE, ylim = ylm, muL = themutList, migL = themigList)

PlotProp("WF", 0.1, 1, muL = themutList, migL = themigList)
PlotProp("WF", 0.1, 0, muL = themutList, migL = themigList)

PlotProp("BD", 0.005, 1, muL = themutList, migL = themigList)
PlotProp("BD", 0.005, 0, addAnalysis = TRUE, ylim = ylm, muL = themutList, migL = themigList)

PlotProp("BD", 0.1, 1, muL = themutList, migL = themigList)
PlotProp("BD", 0.1, 0, addAnalysis = FALSE, muL = themutList, migL = themigList)

PlotProp("DB", 0.005, 1, muL = themutList, migL = themigList)
PlotProp("DB", 0.005, 0, addAnalysis = TRUE, ylim = ylm, muL = themutList, migL = themigList)

PlotProp("DB", 0.1, 1, muL = themutList, migL = themigList)
PlotProp("DB", 0.1, 0, addAnalysis = FALSE, muL = themutList, migL = themigList)



stop()


selList
PlotProp(0.05, 5, 0.01, c(0.4,0.45))  

PlotProp(0.05, 15, 0.01, c(0,1))  

PlotProp(0.05, 30, 0.01, c(0,1))  
# -> This one (above is pretty odd!!)
PlotDensity(0.05, 30, 0.01, c(0,1))  
# But likely problem with NAs

PlotProp(0.05, 5, 0.1, c(0,1))  

PlotProp(0.05, 15, 0.1, c(0.2,0.5))  
# Good one too
PlotDensity(0.05, 15, 0.1, c(0.,1))  

PlotProp(0.05, 30, 0.1, c(0,1))  

PlotProp(0.05, 5, 0.2, c(0,1))  

PlotProp(0.05, 15, 0.2, c(0,1))  
# Increases as well
PlotDensity(0.05, 15, 0.2, c(0,1))  

PlotProp(0.05, 30, 0.2, c(0,1))  


PlotProp(0.1, 15, 0.1, c(0.,0.2))  
PlotProp(0.5, 15, 0.1, c(0.,1))  

PlotProp(0.1, 15, 0.2, c(0,0.2))  
PlotProp(0.5, 15, 0.2, c(0,1))  

#selList <- c(0.05, 0.1, 0.5)
#mBList <- c(5.0, 15.0, 30.0)
#dList <- c(0.01, 0.1, 0.2)

?abline


