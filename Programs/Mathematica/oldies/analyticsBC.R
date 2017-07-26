Power <- function(a,b) return(a^b)

bBDD  <- function(p, sel, mut, m, g, n, d, Idself, Ieself){
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
-(((-1 + mut)*(-(Power(m,2)*Power(-1 + mut,2)) + Idself*(-1 + m)*(-1 + mut)*(m*(-1 + mut) + (-1 + d)*(-1 + g)*mut) - (-1 + d)*(-1 + g)*mut*(-(Ieself*mut) + n + (-1 + Ieself)*mut*n) + m*(-1 + mut)*(-n + mut*(g + d*Ieself - d*g*Ieself + d*(-1 + g)*(-1 + Ieself)*n))))/(-(Power(m,2)*Power(-1 + mut,2)) + Idself*(-1 + m)*(-1 + mut)*(m*(-1 + mut) + mut - d*mut) + (-1 + d)*mut*(1 + mut*(-2 + n))*n - m*(-1 + mut)*(1 + d*mut*(-2 + n))*n)))
}

bBDI  <- function(p, sel, mut, m, g, n, d, Idself, Ieself){
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
(Power(d,2)*(-1 + g)*(-1 + m)*mut*(Idself + Ieself*m - Idself*Ieself*m + (-1 + Idself)*Ieself*(-1 + m)*mut - n) + Power(mut,2)*(m + Idself*(-1 + m)*(-1 + mut) + mut - m*mut - n) + d*((-1 + Idself)*Power(-1 + m,2)*Power(mut,3) + (-1 + m)*Power(mut,2)*(Ieself - g*Ieself + Idself*(1 + (-1 + g)*Ieself - 2*m) + 2*m - n) + m*(Idself + m - Idself*m - n) + mut*(-(Idself*(-1 + g - 2*m)*(-1 + m)) + g*(m - n) + n + m*(-2*m + n))))/(d*(Power(m,2)*Power(-1 + mut,2) - Idself*(-1 + m)*(-1 + mut)*(m*(-1 + mut) + mut - d*mut) - (-1 + d)*mut*(1 + mut*(-2 + n))*n + m*(-1 + mut)*(1 + d*mut*(-2 + n))*n)))
}

cBDD  <- function(p, sel, mut, m, g, n, d, Idself, Ieself){
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
1 - mut)
}

cBDI  <- function(p, sel, mut, m, g, n, d, Idself, Ieself){
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
(Power(mut,2)*(m + Idself*(-1 + m)*(-1 + mut) + mut - m*mut - n) + Power(d,2)*(-1 + m)*mut*((-1 + m)*(-1 + mut)*n + Idself*(-1 + (m + mut - m*mut)*n)) + d*(-(Power(m,2)*(-1 + mut)*(1 + (-1 + mut)*mut)) + mut*(-Power(mut,2) + n) + m*(-1 + mut)*(2*Power(mut,2) + n) + Idself*(-1 + m)*(mut + m*(-1 + mut)*(1 + (-1 + mut)*mut) - Power(mut,2)*(-1 + mut + n))))/(d*(Power(m,2)*Power(-1 + mut,2) - Idself*(-1 + m)*(-1 + mut)*(m*(-1 + mut) + mut - d*mut) - (-1 + d)*mut*(1 + mut*(-2 + n))*n + m*(-1 + mut)*(1 + d*mut*(-2 + n))*n)))
}

bDBD  <- function(p, sel, mut, m, g, n, d, Idself, Ieself){
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
-(((-1 + mut)*(-(Power(m,2)*Power(-1 + mut,2)) + Idself*(-1 + m)*(-1 + mut)*(m*(-1 + mut) + (-1 + d)*(-1 + g)*mut) - (-1 + d)*(-1 + g)*mut*(-(Ieself*mut) + n + (-1 + Ieself)*mut*n) + m*(-1 + mut)*(-n + mut*(g + d*Ieself - d*g*Ieself + d*(-1 + g)*(-1 + Ieself)*n))))/(-(Power(m,2)*Power(-1 + mut,2)) + Idself*(-1 + m)*(-1 + mut)*(m*(-1 + mut) + mut - d*mut) + (-1 + d)*mut*(1 + mut*(-2 + n))*n - m*(-1 + mut)*(1 + d*mut*(-2 + n))*n)))
}

bDBI  <- function(p, sel, mut, m, g, n, d, Idself, Ieself){
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
((-1 + mut)*((-1 + d)*(-1 + g)*Power(Idself,2)*(-1 + Ieself)*Power(-1 + m,2)*mut*(-(d*m) + mut + d*(-1 + m)*mut) + (-1 + d)*m*(-1 + n)*(-m + n) + (1 + d*(-1 + m))*(-1 + m)*Power(mut,2)*((-1 + d)*(-1 + g)*Ieself*(-1 + m) + g*m + n - (g + d*(-1 + g)*(-1 + m) + m)*n) + Idself*(-1 + m)*(-(d*Power(m,2)*(-1 + mut)*mut*(-1 + 2*g + 2*Ieself - 2*g*Ieself + d*(-1 + g)*(-1 + 2*Ieself - n) - n)) + Power(-1 + d,2)*(-1 + g)*mut*(1 + mut - 2*Ieself*mut + (-1 + mut)*n) + (-1 + d)*m*(-1 + n + 2*d*(-1 + g)*mut*(-Ieself + n) - Power(mut,2)*(1 + 2*g*(-1 + Ieself) - 2*Ieself - 2*d*(-1 + g)*(-1 + 2*Ieself - n) + n))) - mut*(g*(m*(1 + d*(-1 + m)*(1 + (-1 + d)*Ieself*(-1 + m) + m)) - Power(1 + d*(-1 + m),2)*(1 + m)*n + Power(1 + d*(-1 + m),2)*Power(n,2)) - (-1 + d)*(-1 + m)*((-1 + n)*n + d*(-1 + m)*(Ieself*m - (1 + m)*n + Power(n,2))))))/((-1 + d)*(-1 + n)*(Power(m,2)*Power(-1 + mut,2) - Idself*(-1 + m)*(-1 + mut)*(m*(-1 + mut) + mut - d*mut) - (-1 + d)*mut*(1 + mut*(-2 + n))*n + m*(-1 + mut)*(1 + d*mut*(-2 + n))*n)))
}

cDBD  <- function(p, sel, mut, m, g, n, d, Idself, Ieself){
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
1 - mut)
}

cDBI  <- function(p, sel, mut, m, g, n, d, Idself, Ieself){
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
((-1 + mut)*((-1 + Idself)*Idself*(1 + d*(-1 + m))*Power(-1 + m,2)*Power(mut,2) + m*(Idself + m - Idself*m - n) - (-1 + m)*mut*(Idself*(-1 + d*(-1 + m)*(-1 + (-1 + Idself)*m)) + n + d*(-1 + m)*n)))/(-(Power(m,2)*Power(-1 + mut,2)) + Idself*(-1 + m)*(-1 + mut)*(m*(-1 + mut) + mut - d*mut) + (-1 + d)*mut*(1 + mut*(-2 + n))*n - m*(-1 + mut)*(1 + d*mut*(-2 + n))*n))
}

bWFD  <- function(p, sel, mut, m, g, n, d, Idself, Ieself){
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
(1 - mut)*((g*(-(1/(1 - (Power(1 + d*(-1 + m),2)*Power(-1 + mut,2))/Power(-1 + d,2))) + 1/(2*mut - Power(mut,2))))/((-1 + d)/(1 - (Power(1 + d*(-1 + m),2)*Power(-1 + mut,2))/Power(-1 + d,2)) + 1/(2*mut - Power(mut,2)) + (d*(-1 + n))/(1 - (Power(-1 + Idself,2)*Power(-1 + m,2)*Power(-1 + mut,2))/Power(-1 + n,2))) + (Ieself - g*Ieself)/n - ((1 - g)*((-1 + d)/(1 - (Power(1 + d*(-1 + m),2)*Power(-1 + mut,2))/Power(-1 + d,2)) + 1/(2*mut - Power(mut,2)) - d/(1 - (Power(-1 + Idself,2)*Power(-1 + m,2)*Power(-1 + mut,2))/Power(-1 + n,2)))*(Ieself - n))/(((-1 + d)/(1 - (Power(1 + d*(-1 + m),2)*Power(-1 + mut,2))/Power(-1 + d,2)) + 1/(2*mut - Power(mut,2)) + (d*(-1 + n))/(1 - (Power(-1 + Idself,2)*Power(-1 + m,2)*Power(-1 + mut,2))/Power(-1 + n,2)))*n)))
}

bWFI  <- function(p, sel, mut, m, g, n, d, Idself, Ieself){
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
((-1 + mut)*(-2*(-1 + 2*d)*Power(m,3)*Power(-1 + mut,2)*(-1 + d*(-1 + g)*Ieself*(-2 + mut)*mut) + d*Power(m,4)*Power(-1 + mut,2)*(-1 + d*(-1 + g)*Ieself*(-2 + mut)*mut) - 2*Idself*Power(-1 + m,2)*(Power(-1 + d,2)*(-1 + g)*(-2 + mut)*mut*(1 + Ieself*(-2 + mut)*mut) - 2*(-1 + d)*m*Power(-1 + mut,2)*(-1 + d*(-1 + g)*Ieself*(-2 + mut)*mut) + d*Power(m,2)*Power(-1 + mut,2)*(-1 + d*(-1 + g)*Ieself*(-2 + mut)*mut)) + Power(Idself,2)*Power(-1 + m,2)*(Power(-1 + d,2)*(-1 + g)*(-2 + mut)*mut*(1 + Ieself*(-2 + mut)*mut) - 2*(-1 + d)*m*Power(-1 + mut,2)*(-1 + d*(-1 + g)*Ieself*(-2 + mut)*mut) + d*Power(m,2)*Power(-1 + mut,2)*(-1 + d*(-1 + g)*Ieself*(-2 + mut)*mut)) + Power(-1 + d,2)*(-1 + g)*(-2 + mut)*mut*(Ieself*(-2 + mut)*mut + 2*n - Power(n,2)) + 2*(-1 + d)*m*(-((-2 + mut)*mut*(Ieself*((-2 + mut)*mut + d*(-1 + 4*mut - 2*Power(mut,2))) + g*(-1 - Ieself*(-2 + mut)*mut + d*Ieself*(1 - 4*mut + 2*Power(mut,2))))) + (2 - 2*d*(-1 + g)*(-2 + mut)*mut)*n + (-1 + d*(-1 + g)*(-2 + mut)*mut)*Power(n,2)) + Power(m,2)*(4 - 2*(3 + g)*mut + (3 + g - 4*Ieself + 4*g*Ieself)*Power(mut,2) - 4*(-1 + g)*Ieself*Power(mut,3) + (-1 + g)*Ieself*Power(mut,4) + Power(d,2)*(-1 + g)*(-2 + mut)*mut*(Ieself*(5 - 12*mut + 6*Power(mut,2)) - (-2 + n)*n) + d*(-4 + (6 + 4*g - 8*Ieself + 8*g*Ieself)*mut - (3 + 2*g - 28*Ieself + 28*g*Ieself)*Power(mut,2) + 24*(-1 + g)*Ieself*Power(mut,3) - 6*(-1 + g)*Ieself*Power(mut,4) - 2*n + Power(n,2)))))/(-2*(-1 + 2*d)*Power(m,3)*Power(-1 + mut,4) + d*Power(m,4)*Power(-1 + mut,4) - 2*Idself*Power(-1 + m,2)*Power(-1 + mut,2)*(-2*(-1 + d)*m*Power(-1 + mut,2) + d*Power(m,2)*Power(-1 + mut,2) + Power(-1 + d,2)*(-2 + mut)*mut) + Power(Idself,2)*Power(-1 + m,2)*Power(-1 + mut,2)*(-2*(-1 + d)*m*Power(-1 + mut,2) + d*Power(m,2)*Power(-1 + mut,2) + Power(-1 + d,2)*(-2 + mut)*mut) + Power(-1 + d,2)*(-2 + mut)*mut*n*(2 - 6*mut + 3*Power(mut,2) + (-1 + 6*mut - 3*Power(mut,2))*n + (-2 + mut)*mut*Power(n,2)) - 2*(-1 + d)*m*Power(-1 + mut,2)*n*(2 + 3*d*(-2 + mut)*mut + (-1 - 3*d*(-2 + mut)*mut)*n + d*(-2 + mut)*mut*Power(n,2)) + Power(m,2)*Power(-1 + mut,2)*(-4 + 6*mut - 3*Power(mut,2) + d*(4 - 6*mut + 3*Power(mut,2) + 2*n - Power(n,2)) + Power(d,2)*(-2 + mut)*mut*n*(3 - 3*n + Power(n,2)))))
}

cWFD  <- function(p, sel, mut, m, g, n, d, Idself, Ieself){
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
1 - mut)
}

cWFI  <- function(p, sel, mut, m, g, n, d, Idself, Ieself){
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
(1 - mut)*(-(((2 + d*(-2 + m))*m*(-(1/(1 - (Power(1 + d*(-1 + m),2)*Power(-1 + mut,2))/Power(-1 + d,2))) + 1/(2*mut - Power(mut,2))))/((-1 + d)*((-1 + d)/(1 - (Power(1 + d*(-1 + m),2)*Power(-1 + mut,2))/Power(-1 + d,2)) + 1/(2*mut - Power(mut,2)) + (d*(-1 + n))/(1 - (Power(-1 + Idself,2)*Power(-1 + m,2)*Power(-1 + mut,2))/Power(-1 + n,2))))) + (Power(Idself,2)*Power(-1 + m,2))/Power(n,2) + (Power(-1 + m,2)*Power(Idself - n,2))/((-1 + n)*Power(n,2)) + Power(m,2)/((-1 + d)*n) + (((-1 + d)/(1 - (Power(1 + d*(-1 + m),2)*Power(-1 + mut,2))/Power(-1 + d,2)) + 1/(2*mut - Power(mut,2)) - d/(1 - (Power(-1 + Idself,2)*Power(-1 + m,2)*Power(-1 + mut,2))/Power(-1 + n,2)))*(2*(-1 + d)*Idself*Power(-1 + m,2) - (-1 + d)*Power(Idself,2)*Power(-1 + m,2) + (-1 + d)*(-2 + n)*n - 2*(-1 + d)*m*(-2 + n)*n + Power(m,2)*(1 + d*(-2 + n)*n)))/((-1 + d)*((-1 + d)/(1 - (Power(1 + d*(-1 + m),2)*Power(-1 + mut,2))/Power(-1 + d,2)) + 1/(2*mut - Power(mut,2)) + (d*(-1 + n))/(1 - (Power(-1 + Idself,2)*Power(-1 + m,2)*Power(-1 + mut,2))/Power(-1 + n,2)))*(-1 + n)*n)))
}