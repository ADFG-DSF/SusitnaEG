model{   
#priors of multinomial likelihood
  for(y in 1:Y){q_m[y, 1:K] ~ ddirch(one)}
  
#priors of poisson likelihood
  for (y in 1:Y) {     
    beta[y,1] <- 0 ;  # zero contrast for baseline age
    for (k in 2:K){  beta[y,k] ~ dunif(-100, 100)} # vague priors
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
  }
}
  