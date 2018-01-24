# Initializations
rm(list = ls())
source("globalGraphParms.R")
# Define Power function used in Mathematica's CForm
Power <- function(a, b) return(a^b)

npts <- 10000 # number of plotting points

mutvals <- c(0, 0.001, 0.01, 0.1, 0.25)
cols <- c(col0, col0001, col001, col01, col025)
ltys <- c(2, rep(1, length(mutvals)-1))
lwds <- rep(3, length(mutvals))

# Critical value of alpha, found with Mathematica
alphac <- function(m, mutation, b = 10, c = 1, n = 4){
  # m emigration probability
  # mutation mutation probability mu
  # b benefits of social interactions
  # c costs of social interactions
  # n deme size
((m*(-1 + mutation) - mutation)*(2 + m*(-1 + mutation) - mutation)*
     ((b - c)*Power(-1 + m,2) + c*n))/
   ((b - c)*(-2 + m)*Power(-1 + m,2)*m*Power(-1 + mutation,2)*n)
}

cexplot <- 1.3
cexlab <- 1.4
cexaxis <- 1

# Initialize plot
filename <- "Pics/SFig_alpha.pdf"
pdf(filename, width = wpdfQ, height = hpdfQ)

par(las = 1, fg = fgcol, bg = bgcol, mar = marpdfQ, cex = cexplot, cex.axis = cexaxis)
par(mgp=mgppdf) # Position of the tick labels

plot(c(0,1), c(0,1), type = "n", 
     asp = 1, xlim = c(0,1), ylim = c(0,1),
     xlab = "", 
     ylab = "", 
     axes = FALSE
)
axis(1, pos = 0)
axis(2, pos = 0)
mtext("Emigration probability m", 1, line = 1.5, cex = cexlab)
mtext(expression(paste("Probability same parent ", alpha)), 2, line = 1.5, las = 3, cex = cexlab)

clip(0, 1, 0, 1)

for(i in seq_along(mutvals)){
  curve(alphac(m = x, mutvals[i]), add = TRUE, from = 0, to = 1, n = npts, 
        col = cols[i], lty = ltys[i], lwd = lwds[i])
}

# Add legends
# Add legend
xleg <- 0.5 # x position
yleg <- 0.6 # y position
legend(xleg, yleg, col = cols, legend = round(mutvals, 5), lty = ltys, lwd = rep(lwds[1], 3), bty = "n", cex = 1)
# Add title to the legend box
text(xleg, yleg, adj = c(-2, 0.5), labels = expression(mu * " ="), cex = cexaxis)

text(x = 0.275, y = 0.9, labels = expression(italic(B*R > C)), cex = cexaxis)
text(x = 0.275, y = 0.2, labels = expression(italic(B*R < C)), cex = cexaxis)
dev.off()
system(paste0("xdg-open ", filename))