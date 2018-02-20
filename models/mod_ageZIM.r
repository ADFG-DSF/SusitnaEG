model{
#PRIORS
#years: 
 for (k in 1 : K) {
  beta[1, k] <- 0
 }
 for (i in 2 : I) { 
  beta[i, 1] <- 0 ; # zero contrast for baseline age
  for (k in 2 : K){ 
   beta[i , k] ~ dnorm(mu_beta[k], tau_beta[k])
  } 
 }
 mu_beta[1] <- 0
 for (k in 2:K){
  mu_beta[k] ~ dnorm(0, 0.0001)
  tau_beta[k] <- 1 / sigma_beta[k] / sigma_beta[k]
  sigma_beta[k] ~ dexp(0.0001)
 }
#samples: 
 for (k in 1 : K) {
  gamma[1, k] <- 0
 }
 for (j in 2 : J) { 
  gamma[j, 1] <- 0 ; # zero contrast for baseline age
  for (k in 2 : K){ 
   gamma[j , k] ~ dnorm(mu_gamma[k], tau_gamma[k])
  } 
 }
 mu_gamma[1] <- 0
 for (k in 2:K){
  mu_gamma[k] ~ dnorm(0, 0.0001)
  tau_gamma[k] <- 1 / sigma_gamma[k] / sigma_gamma[k]
  sigma_gamma[k] ~ dexp(0.0001)
 }

 p0[1, 1] ~ dunif(0, 1)
 p0[2, 1] ~ dunif(0, 1)
 p0[3, 1] ~ dunif(0, 1)
 for (k in 2 : K) {
  p0[1, k] <- 1
  p0[2, k] <- 1
  p0[3, k] <- 1
 }
 for (j in 4 : J) {
  for (k in 1 : K){ 
   p0[j , k] <- 1
  } 
 }

 # Zero inflated Multinomial LIKELIHOOD   
 for (n in 1 : N) {
  X[n,1 : K] ~ dmulti( p[n, 1 : K] , Xsum[n])
  for (k in 1 : K) { # loop around foods
   p[n, k] <- phi0[n, k] / sum(phi0[n, ])
   phi0[n, k] <- obs[n, k] * phi[n, k] + .00001
   log(phi[n, k]) <- beta[year[n], k] + gamma[sample[n], k]
   obs[n, k] ~ dbern(p0[sample[n], k])
   }
 }

 # predictions
 for (n in 1 : N_pred) {
  for (k in 1 : K) { # loop around foods
   p_pred[n, k] <- phi_pred[n, k] / sum(phi_pred[n, ])
   log(phi_pred[n, k]) <- beta[year_pred[n], k] + mu_gamma[k]
  }
 }	
} 