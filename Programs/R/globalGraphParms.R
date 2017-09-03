# Graphical parameters
bgcol <- "transparent"#"transparent"
fgcol <- "black"

par(bg=bgcol, fg=fgcol, col=fgcol, col.axis=fgcol, col.lab=fgcol, col.main=fgcol, col.sub=fgcol)
# http://www.perbang.dk/rgbgradient/ for the gradients
mygradient6 <- c("#234BCD", "#4E579F", "#7A6471", "#A57043", "#D17D15", "black")
mygradient5 <- c("#234BCD", "#4E579F", "#7A6471", "#A57043", "#D17D15")
#mygradient4 <- c("#234BCD", "#5D5B8F", "#976C52", "#D17D15")
mygradient3 <- c("#234BCD", "#7A6471", "#D17D15")

# Rev colors
col025 <- "#FFAA00"
col01 <- "#BF7F3F"
col001 <- "#7F557F"
col0001 <- "#3F2ABF"
mygradient4 <- c(col025, col01, col001, col0001)
col0 <- colnomut <- "#0000FF"

pchs <- c(21,22,23,24,25)


# Make color lighter
Lighten <- function(color, factor=1.2){
  return(rgb(t(factor*col2rgb(color)), maxColorValue=255))
}
# Make color transparent
MakeTransparent<-function(someColor, alpha=20)
{
  newColor <- col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                              blue=curcoldata[3], alpha=alpha, maxColorValue=255)})
}

mgppdf <- c(3, .6, 0) # length and position of the ticks
# Convert cm to in
Cm2In <- function(x){x/2.54}

# Rectangle
rectColor <- gray(1) # Background color
rectLines <- gray(0.6)   # Color of the horizontal lines
rectLlty <- 3          # Line type of the horizontal lines

# Sizes
cexplot <- 1.3
cexlab <- 1.6
cexaxis <- 1.2
dopdf <- TRUE

wpdfQ <- 4
hpdfQ <- 4.25
marpdfQ <- c(3, 3, 0.5, 0)+0.2
hpdfDB <- 5
