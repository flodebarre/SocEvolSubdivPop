# Result exported from Mathematica
theo.beta.GIMBD <- function(mu, m, N=3, D=4, p=1/2){
-((mu*(mu + Power(D,2)*(-1 + m)*(-2 + mu)*mu + D*(m + (-3 + mu)*mu))*(-1 + p)*p)/(D*(-((-1 + D)*mu*(1 + mu*(-1 + N))) + m*(-1 + mu)*(1 + D*mu*(-1 + N)))))
}

theo.beta1.GIMBD <- function(mu, m, N=3, D=4, p=1/2){
((1 - mu)*p*(-((-1 + D)*mu*(1 + mu*(-1 + N*p))) + m*(-1 + mu)*(1 + D*mu*(-1 + N*p))))/(-((-1 + D)*mu*(1 + mu*(-1 + N))) + m*(-1 + mu)*(1 + D*mu*(-1 + N)))
}

theo.beta2.GIMBD <- function(mu, m, N=3, D=4, p=1/2){
(p*(D*m + D*(1 + D*(-1 + m))*Power(mu,3)*(-1 + N)*p - Power(mu,2)*(-1 + p + D*(1 - m + (-3 + D*(2 + 2*m*(-1 + N) - N) + N)*p)) + D*mu*(-1 - m*(1 + p) + D*(1 + m*(-1 + N*p)))))/(D*(-((-1 + D)*mu*(1 + mu*(-1 + N))) + m*(-1 + mu)*(1 + D*mu*(-1 + N))))
}

theo.gamma.GIMBD <- function(mu, m, N=3, D=4, p=1/2){
(mu*(D*(1 + D*(-1 + m))*Power(mu,2)*(-1 + N) + D*m*(-1 + D*N) + mu*(-1 + D*(3 - N + D*(-2 - 2*m*(-1 + N) + N))))*(-1 + p)*p)/(D*(-((-1 + D)*mu*(1 + mu*(-1 + N))) + m*(-1 + mu)*(1 + D*mu*(-1 + N))))
}

theo.gamma1.GIMBD <- function(mu, m, N=3, D=4, p=1/2){
(1 - mu)*p
}

theo.gamma2.GIMBD <- function(mu, m, N=3, D=4, p=1/2){
(p*(D*m + D*(1 + D*(-1 + m))*Power(mu,3)*(-1 + N)*p - Power(mu,2)*(-1 + p + D*(1 - m + (-3 + D*(2 + 2*m*(-1 + N) - N) + N)*p)) + D*mu*(-1 - m*(1 + p) + D*(1 + m*(-1 + N*p)))))/(D*(-((-1 + D)*mu*(1 + mu*(-1 + N))) + m*(-1 + mu)*(1 + D*mu*(-1 + N))))
}

