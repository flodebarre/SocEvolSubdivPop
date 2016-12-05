# Result exported from Mathematica
theo.beta.GIMDB <- function(mu, m, N=3, D=4, p=1/2){
((2 + D*(-2 + m))*m*(-1 + mu)*mu*(-1 + p)*p)/(-((-1 + D)*mu*(1 + mu*(-1 + N))) + m*(-1 + mu)*(1 + D*mu*(-1 + N)))
}

theo.beta1.GIMDB <- function(mu, m, N=3, D=4, p=1/2){
-(((-1 + mu)*p*(-((-1 + D)*mu*(1 + mu*(-1 + N)*p)) + m*(-1 + mu)*(1 + D*mu*(-1 + N)*p)))/(-((-1 + D)*mu*(1 + mu*(-1 + N))) + m*(-1 + mu)*(1 + D*mu*(-1 + N))))
}

theo.beta2.GIMDB <- function(mu, m, N=3, D=4, p=1/2){
((-1 + mu)*p*(-m + mu - D*mu + (1 + D*(-1 + m))*Power(mu,2)*(-1 + N)*p + m*mu*(-1 + 2*p + D*(2 + m*(-1 + p) - (1 + N)*p))))/(-((-1 + D)*mu*(1 + mu*(-1 + N))) + m*(-1 + mu)*(1 + D*mu*(-1 + N)))
}

theo.gamma.GIMDB <- function(mu, m, N=3, D=4, p=1/2){
((-1 + mu)*mu*(m*(2 + D*(-1 + m - N)) + (1 + D*(-1 + m))*mu*(-1 + N))*(-1 + p)*p)/(-((-1 + D)*mu*(1 + mu*(-1 + N))) + m*(-1 + mu)*(1 + D*mu*(-1 + N)))
}

theo.gamma1.GIMDB <- function(mu, m, N=3, D=4, p=1/2){
p - mu*p
}

theo.gamma2.GIMDB <- function(mu, m, N=3, D=4, p=1/2){
((-1 + mu)*p*(-m + mu - D*mu + (1 + D*(-1 + m))*Power(mu,2)*(-1 + N)*p + m*mu*(-1 + 2*p + D*(2 + m*(-1 + p) - (1 + N)*p))))/(-((-1 + D)*mu*(1 + mu*(-1 + N))) + m*(-1 + mu)*(1 + D*mu*(-1 + N)))
}

