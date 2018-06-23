################################################################################
#  
#  RJAGS model
#  
################################################################################
model{
  for (stock in 1:5){
#  	tau.white[stock] ~ dgamma(0.001,0.001)
#	tau.red[stock] <- tau.white[stock] * (1-phi*phi)
#	sigma.white[stock] <- 1 / sqrt(tau.white[stock])
#	sigma.red[stock] <- 1 / sqrt(tau.red[stock])
	log.resid.vec[1:(Y - a.min), stock] <- log.resid[(A+a.min):(Y+A-1), stock]
	lnalpha.vec[1:(Y - a.min), stock] <- lnalpha.y[(A+a.min):(Y+A-1), stock]
	  for (c in (A+a.min):(Y+A-1)) {
		log.R[c, stock] ~ dt(log.R.mean2[c, stock],tau.white,500)
		R[c, stock] <- exp(log.R[c, stock])
		log.R.mean1[c, stock] <- log(S[c-a.max, stock]) + lnalpha[stock] - beta[stock] * S[c-a.max, stock] 
		log.resid[c, stock] <- log(R[c, stock]) - log.R.mean1[c, stock]
		lnalpha.y[c, stock] <- lnalpha[stock] + log.resid[c, stock] 
		}
	  log.R.mean2[A+a.min, stock] <- log.R.mean1[A+a.min, stock] + phi * log.resid.0[stock]
	  for (c in (A+a.min+1):(Y+A-1)) {
		log.R.mean2[c, stock] <- log.R.mean1[c, stock] + phi * log.resid[c-1, stock]
		}
	  lnalpha[stock] ~ dnorm(mu.lnalpha, tau.lnalpha)T(0,) #dnorm(0,1.0E-6)T(0,)
	  beta[stock] ~ dnorm(mu.beta, tau.beta)T(0, ) #dnorm(0,1.0E-2)T(0,)                                                             
	  log.resid.0[stock] ~ dnorm(0,tau.red)T(-3,3) 
	  alpha[stock] <- exp(lnalpha[stock])
	  lnalpha.c[stock] <- lnalpha[stock] + (sigma.white * sigma.white / 2 / (1-phi*phi) )
	  S.max[stock] <- 1 / beta[stock]
	  S.eq[stock] <- lnalpha.c[stock] * S.max[stock]
	  S.msy[stock] <- S.eq[stock] * (0.5 - 0.07*lnalpha.c[stock])
	  U.msy[stock] <- lnalpha.c[stock] * (0.5 - 0.07*lnalpha.c[stock])

	# BROOD YEAR RETURNS W/O SR LINK DRAWN FROM COMMON LOGNORMAL DISTN
	  mean.log.R[stock] ~ dnorm(0,1.0E-4)T(0,)       
	  R.0[stock] <- exp(mean.log.R[stock])
	  for (c in 1:a.max) { 
		log.R[c, stock] ~ dt(mean.log.R[stock],tau.R,500)   
		R[c, stock] <- exp(log.R[c, stock])
		}
	}
	phi ~ dnorm(0,1.0E-4)T(-1,1)
	mu.lnalpha ~ dnorm(0, 1E-6)T(0,)
	mu.beta ~ dnorm(0, 1E-6)T(0,)
	tau.lnalpha ~ dgamma(0.001,0.001)
	tau.beta ~ dgamma(0.001,0.001)
	sigma.lnalpha <- 1 / sqrt(tau.lnalpha)
	sigma.beta <- 1 / sqrt(tau.beta)
	tau.white ~ dgamma(0.001,0.001)
	tau.red <- tau.white * (1-phi*phi)
	sigma.white <- 1 / sqrt(tau.white)
	sigma.red <- 1 / sqrt(tau.red)
	tau.R ~ dgamma(0.001,0.001)      
	sigma.R0 <- 1 / sqrt(tau.R)
       
# GENERATE MLD MATURITY SCHEDULES, ONE PER BROOD YEAR
# MULTIVARIATE LOGISTIC MODEL CONTROLS TIME-TREND OF EXPECTED MATURITY
# GIVEN EXPECTED MATURITY, ANNUAL MATURITY SCHEDULES DIRICHLET DISTRIB AT COHORT (BROOD YEAR) c
  Dscale.age ~ dunif(0.01,1)
  Dsum.age <- 1 / (Dscale.age * Dscale.age)  
  ML1[A] <- 0  
  ML2[A] <- 0
for (a in 1:(A-1)) { 
  ML1[a] ~ dnorm(0,0.0001) 
  ML2[a] ~ dnorm(0,0.0001) 
  }

for (c in 1:(Y+A-1)) {
  for (a in 1:A) {
    logistic.a[c,a] <- exp(ML1[a] + ML2[a] * c)
    pi[c,a] <- logistic.a[c,a] / sum(logistic.a[c,])
    gamma[c,a] <- Dsum.age * pi[c,a]
    g[c,a] ~ dgamma(gamma[c,a],0.1)
    p[c,a] <- g[c,a]/sum(g[c,])
    }
  }

# ASSIGN PRODUCT OF p AND R TO ALL CELLS IN N MATRIX
# c SUBSCRIPT INDEXES BROOD YEAR (COHORT)
for (stock in 1:5){
    for (a in 1:A) {
        for (c in a:(Y + (a - 1))) {
            N.tas[c - (a - 1), (A + 1 - a), stock] <- p[c, (A + 1 - a)] * R[c, stock]
            }
        }
	}

# CALENDAR YEAR AGE COMPOSITION 
  for (y in 1:Y) {
    for (a in 1:A) {
	  N.ta[y,a] <- sum(N.tas[y,a, 1:5])
	  q[y,a] <- N.ta[y,a] / sum(N.ta[y, ])
      }
    }
	
# MULTINOMIAL SCALE SAMPLING ON TOTAL ANNUAL RETURN N
# INDEX y IS CALENDAR YEAR
# MULTIVARIATE LOGISTIC MODEL ALLOWS SAMPLING BIAS
for (y in 1:N.yr.a) {  
  x.a[y, 1:A] ~ dmulti(q.star[y, ], n.a[y])
    for (a in 1:A) {
      q.star[y,a] <- rho[y,a] / sum(rho[y,1:A])
	  log(rho[y,a]) <- log(N.ta[yr.a[y],a] / N.ta[yr.a[y], 1]) + b[x.samp[y], a]
      }
  }
for(a in 1:A){b[1,a] <- 0} #corner point weir
for(s in 2:3){
	b[s,1] <- 0 #zero first age
	for(a in 2:A){
		b[s,a] ~ dnorm(0, 0.0001)
	}
}

# ANNUAL RETURN N
for (y in 1:Y) {
  for (stock in 1:5) {
    N[y, stock] <- sum(N.tas[y,1:A, stock])
  }
}

# DIRICHLET DISTRIBUTED SUBSTOCK COMPOSITION BY CALENDAR YEAR- East
  Dscale.S2 ~ dunif(0.01,1)
  Dsum.S2 <- 1 / (Dscale.S2 * Dscale.S2)
  pi.S2.1p ~ dbeta(0.14,0.86)T(0.03,)
  pi.S2.2p ~ dbeta(0.14,0.72)
  pi.S2.3p ~ dbeta(0.14,0.58)
  pi.S2.4p ~ dbeta(0.14,0.44)
  pi.S2.5p ~ dbeta(0.14,0.30)
  pi.S2.6p ~ dbeta(0.14,0.16)
  pi.S2[1] <- pi.S2.1p
  pi.S2[2] <- pi.S2.2p * (1 - pi.S2[1])
  pi.S2[3] <- pi.S2.3p * (1 - pi.S2[1] - pi.S2[2])
  pi.S2[4] <- pi.S2.3p * (1 - pi.S2[1] - pi.S2[2] - pi.S2[3])
  pi.S2[5] <- pi.S2.3p * (1 - pi.S2[1] - pi.S2[2] - pi.S2[3] - pi.S2[4])
  pi.S2[6] <- pi.S2.3p * (1 - pi.S2[1] - pi.S2[2] - pi.S2[3] - pi.S2[4] - pi.S2[5])	
  pi.S2[7] <- 1 -  pi.S2[1] - pi.S2[2] - pi.S2[3] - pi.S2[4] - pi.S2[5] - pi.S2[6]
for (trib in 1:7) {
    gamma.S2[trib] <- Dsum.S2 * pi.S2[trib]
    for (y in 1:Y) {
      g.S2[y,trib] ~ dgamma(gamma.S2[trib],0.1)
      p.S2[y,trib] <- g.S2[y,trib]/sum(g.S2[y,])
      }
    }

# SUBSTOCK COMPOSITIONs BY CALENDAR YEAR- Talketna
  Bscale.S3 ~ dunif(0.01,1)
  Bsum.S3 <- 1 / (Bscale.S3 * Bscale.S3)
  pi.S3 ~ dbeta(1,1)
  B1.S3 <- Bsum.S3 * pi.S3; 
  B2.S3 <- Bsum.S3 - B1.S3;
  for(y in 1:Y){                                                    
      p.S3[y, 1] ~ dbeta(B1.S3,B2.S3)
	  p.S3[y, 2] <- 1 - p.S3[y, 1]
      } 
	  
# DIRICHLET DISTRIBUTED SUBSTOCK COMPOSITIONs BY CALENDAR YEAR- Yentna
  Dscale.S4 ~ dunif(0.01,1)
  Dsum.S4 <- 1 / (Dscale.S4 * Dscale.S4)
  pi.S4.1p ~ dbeta(0.25,0.75)T(0.03,)
  pi.S4.2p ~ dbeta(0.25,0.50)
  pi.S4.3p ~ dbeta(0.25,0.25)
  pi.S4[1] <- pi.S4.1p
  pi.S4[2] <- pi.S4.2p * (1 - pi.S4[1])
  pi.S4[3] <- pi.S4.3p * (1 - pi.S4[1] - pi.S4[2])
  pi.S4[4] <- 1 -  pi.S4[1] - pi.S4[2] - pi.S4[3]
for (trib in 1:4) {
    gamma.S4[trib] <- Dsum.S4 * pi.S4[trib]
    for (y in 1:Y) {
      g.S4[y,trib] ~ dgamma(gamma.S4[trib],0.1)
      p.S4[y,trib] <- g.S4[y,trib]/sum(g.S4[y,])
      }
    }
	
# DIRICHLET DISTRIBUTED SUBSTOCK COMPOSITIONs BY CALENDAR YEAR- other main
  Dscale.S5 ~ dunif(0.01,1)
  Dsum.S5 <- 1 / (Dscale.S5 * Dscale.S5)
  pi.S5.1p ~ dbeta(0.33,0.67)T(0.03,)
  pi.S5.2p ~ dbeta(0.33,0.33)
  pi.S5[1] <- pi.S5.1p
  pi.S5[2] <- pi.S5.2p * (1 - pi.S5[1])
  pi.S5[3] <- 1 -  pi.S5[1] - pi.S5[2]
for (trib in 1:3) {
    gamma.S5[trib] <- Dsum.S5 * pi.S5[trib]
    for (y in 1:Y) {
      g.S5[y,trib] ~ dgamma(gamma.S5[trib],0.1)
      p.S5[y,trib] <- g.S5[y,trib]/sum(g.S5[y,])
      }
    }

# MULTINOMIAL COUNTS OF RADIOS TRACKED TO INDIVIDUAL TRIBS
for (y in 1:Y) { 
    tele.S2[y, ] ~  dmulti(p.S2[y, ], Ntele.S2[y])
    tele.S3[y, ] ~  dmulti(p.S3[y, ], Ntele.S3[y])
	tele.S4[y, ] ~  dmulti(p.S4[y, ], Ntele.S4[y])
	tele.S5[y, ] ~  dmulti(p.S5[y, ], Ntele.S5[y])
}

# AIR SURVEY COUNTS W LOGNORMAL ERRORS
for (stock in 1:5){
	theta.mean[stock] ~ dunif(0.1, 0.9) #dbeta(1, 1)
	Bscale.theta[stock] ~ dunif(0.01,1)
	Bsum.theta[stock] <- 1 / (Bscale.theta[stock] * Bscale.theta[stock])
	B1.theta[stock] <- Bsum.theta[stock] * theta.mean[stock]; 
	B2.theta[stock] <- Bsum.theta[stock] - B1.theta[stock];
	tau.air[stock] ~ dgamma(0.1,0.01)
	sigma.air[stock] <- 1 / sqrt(tau.air[stock])
}

# DESHKA survey data
# one trib in the stock
theta.S1 ~ dbeta(B1.theta[1], B2.theta[1])
for(y in 1:Y){
	log.t1S1[y] <- log(theta.S1 * S[y, 1])
	air.S1[y] ~ dlnorm(log.t1S1[y], tau.air[1])
	}
	
for(trib in 1:7) {
	theta.S2[trib] ~ dbeta(B1.theta[2], B2.theta[2])
	for(y in 1:Y){
	log.tpS2[y, trib] <- log(theta.S2[trib] * p.S2[y, trib] * S[y, 2])
	air.S2[y, trib] ~ dlnorm(log.tpS2[y, trib], tau.air[2])
	}
}

for(trib in 1:2) {
	theta.S3[trib] ~ dbeta(B1.theta[3], B2.theta[3])
	for(y in 1:Y){
	log.tpS3[y, trib] <- log(theta.S3[trib] * p.S3[y, trib] * S[y, 3])
	air.S3[y, trib] ~ dlnorm(log.tpS3[y, trib], tau.air[3])
	}
}

for(trib in 1:4) {
	theta.S4[trib] ~ dbeta(B1.theta[4], B2.theta[4])
	for(y in 1:Y){
	log.tpS4[y, trib] <- log(theta.S4[trib] * p.S4[y, trib] * S[y, 4])
	air.S4[y, trib] ~ dlnorm(log.tpS4[y, trib], tau.air[4])
	}
}

for(trib in 1:3) {
	theta.S5[trib] ~ dbeta(B1.theta[5], B2.theta[5])
	for(y in 1:Y){
	log.tpS5[y, trib] <- log(theta.S5[trib] * p.S5[y, trib] * S[y, 5])
	air.S5[y, trib] ~ dlnorm(log.tpS5[y, trib], tau.air[5])
	}
}
   
# DESHKA WEIR COUNTS W (SMALL) LOGNORMAL ERRORS
# DETECTABILITY = 1, one trib in the stock.;
  tau.weir ~ dgamma(0.01,0.1)
  sigma.weir <- 1 / sqrt(tau.weir)
  for (y in 1:Y) {
    log.11S1[y] <- log(S[y, 1])
    weir.deshka[y] ~ dlnorm(log.11S1[y], tau.weir)
    }

# INRIVER RUN AND HARVESTS ESTIMATED
for (y in 1:Y) {
  mu.Hmarine[y] ~ dbeta(0.5,0.5)
  Hmarine[y] <- mu.Hmarine[y] * sum(N[y, ])
  logHm[y] <- log(Hmarine[y])
  tau.logHm[y] <- 1 / log(cv.Hm[y]*cv.Hm[y] + 1)
  Hm.hat[y] ~ dlnorm(logHm[y],tau.logHm[y])             

  p.small3[y] ~ dbeta(1,1)
  p.small4[y] ~ dbeta(1,1)
  small3[y, 1] ~ dbinom(p.small3[y], small3[y, 2])
  small4[y, 1] ~ dbinom(p.small4[y], small4[y, 2])
  tau.logHa[y] <- 1 / log(cv.Ha[y]*cv.Ha[y] + 1)
  tau.logMR[y] <- 1 / log(cv.MR[y]*cv.MR[y] + 1)
  for (stock in 1:5){
    IR[y, stock] <- N[y, stock] * (1 - mu.Hmarine[y]) * (1 - q[y, 1] * p.small3[y]) * (1 - q[y, 2] * p.small4[y]) 
    logIR[y, stock] <- log(IR[y, stock])
    MR[y, stock] ~ dlnorm(logIR[y, stock], tau.logMR[y])    
	mu.Habove[y, stock] ~ dbeta(0.5,0.5)
	Habove[y, stock] <- mu.Habove[y, stock] * IR[y, stock]
	logHa[y, stock] <- log(Habove[y, stock])
	Ha.hat[y, stock] ~ dlnorm(logHa[y, stock], tau.logHa[y])       
	S[y, stock] <- max(IR[y, stock] - Habove[y, stock], 1)
    }
  }
} 