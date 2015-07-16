
# t = 0
# T and r measured using consistent time
european.call <- function(S,K,T,r,sigma) {
  d1 = bsm.d1(S,K,T,r,sigma)
  d2 = bsm.d2(S,K,T,r,sigma)
  S*pnorm(d1) - K*exp(-r*T)*pnorm(d2)
}
european.put <- function(S,K,T,r,sigma) {
  d1 = bsm.d1(S,K,T,r,sigma)
  d2 = bsm.d2(S,K,T,r,sigma)
  K*exp(-r*T)*pnorm(-d2) - S*pnorm(-d1) 
}
digital.call <- function(S,K,T,r,sigma) {
  d2 = bsm.d2(S,K,T,r,sigma)
  K*exp(-r*T)*pnorm(d2)
}

bsm.d1 <- function(S,K,T,r,sigma) {
  (log(S/(K*exp(-r*T))) + (sigma^2)*T/2)/(sigma*sqrt(T))
}

bsm.d2 <- function(S,K,T,r,sigma) {
  bsm.d1(S,K,T,r,sigma) - sigma*sqrt(T)
}


