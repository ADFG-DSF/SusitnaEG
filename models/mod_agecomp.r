    model
    {
	
    # PRIORS
       alpha[1] <- 0; # zero contrast for baseline age
       for (k in 2 : K) { 
          alpha[k] ~ dnorm(0, 0.00001) # vague priors
       } 
    # Loop around years:
       for (k in 1 : K){ 
          beta[1, k] <- 0 
       } # corner-point contrast with first year 
       for (i in 2 : I) { 
          beta[i, 1] <- 0 ; # zero contrast for baseline age
          for (k in 2 : K){ 
             beta[i, k] ~ dnorm(0, 0.00001) # vague priors
          } 
       }
    # Loop around samples:
       for (k in 1 : K){ 
          gamma[1, k] <- 0 # corner-point contrast with first sample 
       } 
       for (j in 2 : J) { 
          gamma[j, 1] <- 0 ; # zero contrast for baseline age
          for ( k in 2 : K){ 
             gamma[j, k] ~ dnorm(0, 0.00001) # vague priors
          } 
       }

    # LIKELIHOOD   
	for (n in 1 : N) {
    # Multinomial response
			X[n,1 : K] ~ dmulti( p[n, 1 : K] , Xsum[n])
				for (k in 1 : K) { # loop around foods
					p[n, k] <- phi[n, k] / sum(phi[n, ])
					log(phi[n, k]) <- alpha[k] + beta[year[n], k] + gamma[sample[n], k]
				}
		}
    } 