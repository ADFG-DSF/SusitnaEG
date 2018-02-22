model{   
#priors of multinomial likelihood
  for(y in 1:Y){q_m[y, 1:K] ~ ddirch(one)}
  
#priors of poisson likelihood
  for (y in 1:Y) {     
    beta[y,1] <- 0 ;  # zero contrast for baseline age
    for (k in 2:K){  beta[y,k] ~ dunif(-100, 100)} # vague priors
  }

#priors of poisson likelihood with missing age3's
  for (y in 1:Y) { 
	beta_miss[y, 1] <- 0 ; # zero contrast for baseline age
	for (k in 2:K){ 
	beta_miss[y, k] ~ dnorm(mu_beta[k], tau_beta[k])
	} 
  }
 mu_beta[1] <- 0
 for (k in 2:K){
  mu_beta[k] ~ dnorm(0, 0.0001)
  tau_beta[k] <- 1 / sigma_beta[k] / sigma_beta[k]
  sigma_beta[k] ~ dexp(0.0001)
 }
 
 #priors of poisson likelihood with missing age3's and extra data
  for (y in 1:Y) { 
	beta_all[y, 1] <- 0 ; # zero contrast for baseline age
	for (k in 2:K){ 
	beta_all[y, k] ~ dnorm(mu_betaall[k], tau_betaall[k])
	} 
  }
 mu_betaall[1] <- 0
 for (k in 2:K){
  mu_betaall[k] ~ dnorm(0, 0.0001)
  tau_betaall[k] <- 1 / sigma_betaall[k] / sigma_betaall[k]
  sigma_betaall[k] ~ dexp(0.0001)
 }

# LIKELIHOOD
  for (y in 1:Y) {     # loop around years 
# Multinomial response
         X_m[y,] ~ dmulti(q_m[y,], Xsum[y]);
# Poisson regressions relative to baseline
        lambda[y] ~ dnorm(0,0.00001); # vague priors 
        for (k in 1:K) {     # loop around ages
          X_p[y,k] ~ dpois(mu[y,k])
          log(mu[y,k]) <- lambda[y] + beta[y,k]
		  q_p[y, k] <- phi[y, k] / sum(phi[y, ])
		  log(phi[y, k]) <- beta[y,k]
       }
	   
# Poisson regressions relative to baseline with missing data
        lambda_miss[y] ~ dnorm(0,0.00001); # vague priors 
        for (k in 1:K) {     # loop around ages
          X_pmiss[y,k] ~ dpois(mu_miss[y,k])
          log(mu_miss[y,k]) <- lambda_miss[y] + beta_miss[y,k]
		  q_pmiss[y, k] <- phi_miss[y, k] / sum(phi_miss[y, ])
		  log(phi_miss[y, k]) <- beta_miss[y,k]
       }
    }
	
  for (n in 1:N) {     # loop around years 
# Poisson regressions relative to baseline with missing data and extra data
        lambda_all[n] ~ dnorm(0,0.00001); # vague priors 
        for (k in 1:K) {     # loop around ages
          X_pall[n,k] ~ dpois(mu_all[n,k])
          log(mu_all[n,k]) <- lambda_all[n] + beta_all[year[n],k]
       }
    } 
  for (y in 1:Y) {     # loop around years 
		for(k in 1:K){
		  q_pall[y, k] <- phi_all[y, k] / sum(phi_all[y, ])
		  log(phi_all[y, k]) <- beta_all[y,k]
       }
    } 	
  }