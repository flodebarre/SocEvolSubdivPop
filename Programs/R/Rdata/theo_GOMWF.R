# Result exported from Mathematica
theo.beta.GIMWF <- function(mu, m, N=3, D=4, p=1/2){
-((D*(-2 + mu)*(-1 + mu)*mu*((2 + D*(-2 + m))*m - 2*Power(1 + D*(-1 + m),2)*mu + Power(1 + D*(-1 + m),2)*Power(mu,2))*N*(-1 + p)*p)/(Power(-1 + D,2)*(-2 + mu)*mu*(-1 + (-2 + mu)*mu*(-1 + N)) - 2*(-1 + D)*m*Power(-1 + mu,2)*(-1 + D*(-2 + mu)*mu*(-1 + N)) + D*Power(m,2)*Power(-1 + mu,2)*(-1 + D*(-2 + mu)*mu*(-1 + N))))
}

theo.beta1.GIMWF <- function(mu, m, N=3, D=4, p=1/2){
D*(1 - mu)*N*p*(-(((-D + (-1 + D)/(1 - (Power(1 + D*(-1 + m),2)*Power(-1 + mu,2))/Power(-1 + D,2)) + 1/(2*mu - Power(mu,2)))*(-1 + p))/((-1 + D)/(1 - (Power(1 + D*(-1 + m),2)*Power(-1 + mu,2))/Power(-1 + D,2)) + 1/(2*mu - Power(mu,2)) + D*(-1 + N))) + p)
}

theo.beta2.GIMWF <- function(mu, m, N=3, D=4, p=1/2){
(D*(-1 + mu)*N*p*(Power(-1 + D,2)*(-2 + mu)*mu*(-1 + (-2 + mu)*mu*(-1 + N)*p) - 2*(-1 + D)*m*(-1 + (-2 + mu)*mu*(-p + D*(-1 - (-2 + mu)*mu*p + Power(-1 + mu,2)*N*p))) + D*Power(m,2)*(-1 + (-2 + mu)*mu*(-p + D*(-1 - (-2 + mu)*mu*p + Power(-1 + mu,2)*N*p)))))/(Power(-1 + D,2)*(-2 + mu)*mu*(-1 + (-2 + mu)*mu*(-1 + N)) - 2*(-1 + D)*m*Power(-1 + mu,2)*(-1 + D*(-2 + mu)*mu*(-1 + N)) + D*Power(m,2)*Power(-1 + mu,2)*(-1 + D*(-2 + mu)*mu*(-1 + N)))
}

theo.gamma.GIMWF <- function(mu, m, N=3, D=4, p=1/2){
(D*(1 - mu)*(-2 + mu)*mu*N*(2*Power(1 + D*(-1 + m),2)*mu*(-1 + N) - Power(1 + D*(-1 + m),2)*Power(mu,2)*(-1 + N) - (2 + D*(-2 + m))*m*(-1 + D*N))*(-1 + p)*p)/(Power(-1 + D,2)*(-2 + mu)*mu*(-1 + (-2 + mu)*mu*(-1 + N)) - 2*(-1 + D)*m*Power(-1 + mu,2)*(-1 + D*(-2 + mu)*mu*(-1 + N)) + D*Power(m,2)*Power(-1 + mu,2)*(-1 + D*(-2 + mu)*mu*(-1 + N)))
}

theo.gamma1.GIMWF <- function(mu, m, N=3, D=4, p=1/2){
-(D*(-1 + mu)*N*p)
}

theo.gamma2.GIMWF <- function(mu, m, N=3, D=4, p=1/2){
(D*(-1 + mu)*N*p*(Power(-1 + D,2)*(-2 + mu)*mu*(-1 + (-2 + mu)*mu*(-1 + N)*p) - 2*(-1 + D)*m*(-1 + (-2 + mu)*mu*(-p + D*(-1 - (-2 + mu)*mu*p + Power(-1 + mu,2)*N*p))) + D*Power(m,2)*(-1 + (-2 + mu)*mu*(-p + D*(-1 - (-2 + mu)*mu*p + Power(-1 + mu,2)*N*p)))))/(Power(-1 + D,2)*(-2 + mu)*mu*(-1 + (-2 + mu)*mu*(-1 + N)) - 2*(-1 + D)*m*Power(-1 + mu,2)*(-1 + D*(-2 + mu)*mu*(-1 + N)) + D*Power(m,2)*Power(-1 + mu,2)*(-1 + D*(-2 + mu)*mu*(-1 + N)))
}
