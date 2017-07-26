Power <- function(a,b) return(a^b)

QinM  <- function(p, sel, mut, m, g, n, d, Idself, Ieself){
## Arguments:
#  p   mutation bias
#  sel intensity of selection
#  mut mutation probability
#  m   emigration probability
#  g   proportion of interactions out of the group (interaction equivalent of m)
#  n   deme size
#  d   number of demes
#  Idself whether reproduction in site where the parent is
#  Ieself whether interactions with oneself
return(
((-1 + mut)*(m*(Idself + m - Idself*m - n) + mut*(-Power(m,2) + Idself*(-1 + d - d*m + Power(m,2)) + n + d*(-1 + m)*n)))/(-(Power(m,2)*Power(-1 + mut,2)) + Idself*(-1 + m)*(-1 + mut)*(m*(-1 + mut) + mut - d*mut) + (-1 + d)*mut*(1 + mut*(-2 + n))*n - m*(-1 + mut)*(1 + d*mut*(-2 + n))*n))
}

QoutM  <- function(p, sel, mut, m, g, n, d, Idself, Ieself){
## Arguments:
#  p   mutation bias
#  sel intensity of selection
#  mut mutation probability
#  m   emigration probability
#  g   proportion of interactions out of the group (interaction equivalent of m)
#  n   deme size
#  d   number of demes
#  Idself whether reproduction in site where the parent is
#  Ieself whether interactions with oneself
return(
(m*(-1 + mut)*(m + Idself*(-1 + m)*(-1 + mut) + mut - m*mut - n))/(-(Power(m,2)*Power(-1 + mut,2)) + Idself*(-1 + m)*(-1 + mut)*(m*(-1 + mut) + mut - d*mut) + (-1 + d)*mut*(1 + mut*(-2 + n))*n - m*(-1 + mut)*(1 + d*mut*(-2 + n))*n))
}

QinWF  <- function(p, sel, mut, m, g, n, d, Idself, Ieself){
## Arguments:
#  p   mutation bias
#  sel intensity of selection
#  mut mutation probability
#  m   emigration probability
#  g   proportion of interactions out of the group (interaction equivalent of m)
#  n   deme size
#  d   number of demes
#  Idself whether reproduction in site where the parent is
#  Ieself whether interactions with oneself
return(
((-1 + d)/(1 - (Power(1 + d*(-1 + m),2)*Power(-1 + mut,2))/Power(-1 + d,2)) + 1/(2*mut - Power(mut,2)) - d/(1 - (Power(-1 + Idself,2)*Power(-1 + m,2)*Power(-1 + mut,2))/Power(-1 + n,2)))/((-1 + d)/(1 - (Power(1 + d*(-1 + m),2)*Power(-1 + mut,2))/Power(-1 + d,2)) + 1/(2*mut - Power(mut,2)) + (d*(-1 + n))/(1 - (Power(-1 + Idself,2)*Power(-1 + m,2)*Power(-1 + mut,2))/Power(-1 + n,2))))
}

QoutWF  <- function(p, sel, mut, m, g, n, d, Idself, Ieself){
## Arguments:
#  p   mutation bias
#  sel intensity of selection
#  mut mutation probability
#  m   emigration probability
#  g   proportion of interactions out of the group (interaction equivalent of m)
#  n   deme size
#  d   number of demes
#  Idself whether reproduction in site where the parent is
#  Ieself whether interactions with oneself
return(
(-(1/(1 - (Power(1 + d*(-1 + m),2)*Power(-1 + mut,2))/Power(-1 + d,2))) + 1/(2*mut - Power(mut,2)))/((-1 + d)/(1 - (Power(1 + d*(-1 + m),2)*Power(-1 + mut,2))/Power(-1 + d,2)) + 1/(2*mut - Power(mut,2)) + (d*(-1 + n))/(1 - (Power(-1 + Idself,2)*Power(-1 + m,2)*Power(-1 + mut,2))/Power(-1 + n,2))))
}