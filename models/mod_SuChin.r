################################################################################
#  
#  RJAGS model
#  
################################################################################
model{
  for (s in 1:SG){
 	tau.white[s] ~ dgamma(0.001,0.001)
	tau.red[s] <- tau.white[s] * (1-phi[s]*phi[s])
	sigma.white[s] <- 1 / sqrt(tau.white[s])
	sigma.red[s] <- 1 / sqrt(tau.red[s])
	log.resid.vec[1:(Y - a.min), s] <- log.resid[(A+a.min):(Y+A-1), s]
	lnalpha.vec[1:(Y - a.min), s] <- lnalpha.y[(A+a.min):(Y+A-1), s]
	  for (c in (A+a.min):(Y+A-1)) {
		log.R[c, s] ~ dt(log.R.mean2[c, s],tau.white[s],500)
		R[c, s] <- exp(log.R[c, s])
		log.R.mean1[c, s] <- log(S[c-a.max, s]) + lnalpha[s] - beta[s] * S[c-a.max, s]  #Eq. 1
		log.resid[c, s] <- log(R[c, s]) - log.R.mean1[c, s]  #Eq. 3
		lnalpha.y[c, s] <- lnalpha[s] + log.resid[c, s] 
		}
	  log.R.mean2[A+a.min, s] <- log.R.mean1[A+a.min, s] + phi[s] * log.resid.0[s]  #Eq. 2
	  for (c in (A+a.min+1):(Y+A-1)) {
		log.R.mean2[c, s] <- log.R.mean1[c, s] + phi[s] * log.resid[c-1, s]
		}
	  lnalpha[s] ~ dnorm(mu.lnalpha, tau.lnalpha)T(0,)  
	  beta[s] ~ dnorm(0, 1.0E-2)T(0,)
	  log.resid.0[s] ~ dnorm(0,tau.red[s])T(-3,3) 
	  alpha[s] <- exp(lnalpha[s])
	  lnalpha.c[s] <- lnalpha[s] + (sigma.white[s] * sigma.white[s] / 2 / (1-phi[s]*phi[s]))  #Eq. 28
	  phi[s] ~ dunif(-0.95, 0.95)
	  S.max[s] <- 1 / beta[s]  #Eq. 31
	  S.eq[s] <- lnalpha.c[s] * S.max[s]  #Eq. 32
	  S.msy[s] <- S.eq[s] * (0.5 - 0.07*lnalpha.c[s])  #Eq. 27
	  U.msy[s] <- lnalpha.c[s] * (0.5 - 0.07*lnalpha.c[s])  #Eq. 30

	# BROOD YEAR RETURNS W/O SR LINK DRAWN FROM COMMON LOGNORMAL DISTN
	  mean.log.R[s] ~ dnorm(0,1.0E-4)T(0,)       
	  R.0[s] <- exp(mean.log.R[s])
	  for (c in 1:a.max) { 
		log.R[c, s] ~ dt(mean.log.R[s],tau.R,500)   
		R[c, s] <- exp(log.R[c, s])
		}
	}
	#Hierarchical lnalpha
	mu.lnalpha ~ dnorm(0, 1E-6)T(0,)
	tau.lnalpha ~ dgamma(2,1)
	sigma.lnalpha <- 1 / sqrt(tau.lnalpha)
	tau.R ~ dgamma(0.001,0.001)      
	sigma.R0 <- 1 / sqrt(tau.R)
       
### GENERATE MATURITY SCHEDULES, ONE PER BROOD YEAR
# MULTIVARIATE LOGISTIC MODEL CONTROLS TIME-TREND OF EXPECTED MATURITY
# GIVEN EXPECTED MATURITY, ANNUAL MATURITY SCHEDULES DIRICHLET DISTRIB AT COHORT (BROOD YEAR) c
  Dscale.age ~ dunif(0.07,1)
  Dsum.age <- 1 / (Dscale.age * Dscale.age)
  ML1[A] <- 0  
  ML2[A] <- 0
for (a in 1:(A-1)) { 
  ML1[a] ~ dnorm(0,0.0001) 
  ML2[a] ~ dnorm(0,0.0001) 
  }

for (c in 1:(Y+A-1)) {
  for (a in 1:A) {
    logistic.a[c,a] <- exp(ML1[a] + ML2[a] * c) #Eq. 5.2
    pi[c,a] <- logistic.a[c,a] / sum(logistic.a[c,])
    gamma[c,a] <- Dsum.age * pi[c,a]  #Eq. 5.1
    g[c,a] ~ dgamma(gamma[c,a],0.1)
    p[c,a] <- g[c,a]/sum(g[c,])
    }
  }

# ASSIGN PRODUCT OF p AND R TO ALL CELLS IN N MATRIX
for (s in 1:SG){
    for (a in 1:A) {
        for (c in a:(Y + (a - 1))) {
            N.tas[c - (a - 1), (A + 1 - a), s] <- p[c, (A + 1 - a)] * R[c, s]  #Eq. 6
            }
        }
	}

### CALENDAR YEAR AGE COMPOSITION 
  for (y in 1:Y) {
    for (a in 1:A) {
	  N.ta[y,a] <- sum(N.tas[y,a, 1:SG])
	  q[y,a] <- N.ta[y,a] / sum(N.ta[y, ])  #Eq. 26
      }
    }
	
# MULTINOMIAL SCALE SAMPLING ON TOTAL ANNUAL RETURN N
# INDEX y IS CALENDAR YEAR
# MULTIVARIATE LOGISTIC MODEL ADJUSTS FOR SAMPLE LOCATION
for (j in 1:J) {  
  x.a[j, 1:A] ~ dmulti(q.star[j, ], n.a[j])  #Eq. 24
    for (a in 1:A) {
      q.star[j,a] <- rho[j,a] / sum(rho[j,1:A])  #Eq. 25
	  log(rho[j,a]) <- log(N.ta[yr.a[j],a] / N.ta[yr.a[j], 1]) + b[x.stock[j], a]
      }
  }
for(a in 1:A){b0[1,a] <- 0} #Deshka baseline
for(s in 2:SG){b0[s,1] <- 0 for(a in 2:A){b0[s,a] ~ dnorm(0, 0.0001)}}
for(s in 1:SG){for(a in 1:A){b[s,a] <- b0[s,a] - mean(b0[,a])}}


# ANNUAL RETURN N
for (y in 1:Y) {
  for (s in 1:SG) {
    N[y, s] <- sum(N.tas[y,1:A, s])  #Eq. 7
  }
}	

### STOCK COMPOSITION ###
### MULTIVARIATE LOGISTIC MODEL CONTROLS TIME-TREND OF STOCK COMPOSITION
### GIVEN EXPECTED COMPOSITION, ANNUAL COMPOSITION DIRICHLET DISTRIB AT YEAR y.
### note p.S# is rho.# in report
# East Susitna, T_s=7
  Dscale.S2 ~ dunif(0.07,1)
  Dsum.S2 <- 1 / (Dscale.S2 * Dscale.S2)
  ML1.S2[6] <- 0  
  ML2.S2[6] <- 0
for (t in 1:5) { 
  ML1.S2[t] ~ dnorm(0,0.0001) 
  ML2.S2[t] ~ dnorm(0,0.0001) 
  }

for (y in 1:Y) {
	for (t in 1:6) {
	  logistic.S2[y, t] <- exp(ML1.S2[t] + ML2.S2[t] * y)
      pi.S2[y, t] <- logistic.S2[y, t] / sum(logistic.S2[y, ])
      gamma.S2[y, t] <- Dsum.S2 * pi.S2[y, t]
      g.S2[y, t] ~ dgamma(gamma.S2[y, t], 0.1)
      p.S2s[y, t] <- g.S2[y, t]/sum(g.S2[y, ])
	  p.S2[y, t] <- p.S2s[y, t] * (1 - p.S2o[y]) #Eq. 14 elements 1:(T_s-1)
      }
	  p.S2[y, 7] <- p.S2o[y]  #Eq. 14  element T_s
    }

# Talkeetna, T_s=3
  Dscale.S3 ~ dunif(0.07,1)
  Dsum.S3 <- 1 / (Dscale.S3 * Dscale.S3)
  ML1.S3[2] <- 0  
  ML2.S3[2] <- 0
  ML1.S3[1] ~ dnorm(0,0.0001)
  ML2.S3[1] ~ dnorm(0,0.0001) 

for (y in 1:Y) {
	for (t in 1:2) {
	  logistic.S3[y, t] <- exp(ML1.S3[t] + ML2.S3[t] * y)
      pi.S3[y, t] <- logistic.S3[y, t] / sum(logistic.S3[y, ])
      gamma.S3[y, t] <- Dsum.S3 * pi.S3[y, t]
      g.S3[y, t] ~ dgamma(gamma.S3[y, t], 0.1)
      p.S3s[y, t] <- g.S3[y, t]/sum(g.S3[y, ])
	  p.S3[y, t] <- p.S3s[y, t] * (1 - p.S3o[y])
      }
	  p.S3[y, 3] <- p.S3o[y]
    }

# Yentna, T_s=5
  Dscale.S4 ~ dunif(0.07,1)
  Dsum.S4 <- 1 / (Dscale.S4 * Dscale.S4)
  ML1.S4[4] <- 0  
  ML2.S4[4] <- 0
for (t in 1:3) { 
  ML1.S4[t] ~ dnorm(0,0.0001) 
  ML2.S4[t] ~ dnorm(0,0.0001) 
  }

for (y in 1:Y) {
	for (t in 1:4) {
	  logistic.S4[y, t] <- exp(ML1.S4[t] + ML2.S4[t] * y)
      pi.S4[y, t] <- logistic.S4[y, t] / sum(logistic.S4[y, ])
      gamma.S4[y, t] <- Dsum.S4 * pi.S4[y, t]
      g.S4[y, t] ~ dgamma(gamma.S4[y, t], 0.1)
	  p.S4s[y, t] <- g.S4[y, t]/sum(g.S4[y, ])
	  p.S4[y, t] <- p.S4s[y, t] * (1 - p.S4o[y])
      }
	  p.S4[y, 5] <- p.S4o[y]
    }	
	
# MULTINOMIAL COUNTS OF RADIOS TRACKED TO SURVEYED AREAS
for (y in 1:Y) { 
    tele.S2[y, 1:6] ~  dmulti(p.S2s[y, ], Ntele.S2[y] - tele.S2[y, 7])  #Eq. 21
    tele.S3[y, 1:2] ~  dmulti(p.S3s[y, ], Ntele.S3[y] - tele.S3[y, 3])
	tele.S4[y, 1:4] ~  dmulti(p.S4s[y, ], Ntele.S4[y] - tele.S4[y, 5])
}

for(s in 1:(SG - 1)){
  p.So.mean[s] ~ dbeta(1, 1)
  Bscale.So[s] ~ dunif(0.07, 1)
  Bsum.So[s] <- 1 / Bscale.So[s] / Bscale.So[s]
  B1.So[s] <- Bsum.So[s] * p.So.mean[s]
  B2.So[s] <- Bsum.So[s] - B1.So[s]
}

# MULTINOMIAL COUNTS OF RADIOS TRACKED TO UNSURVEYED AREAS
for (y in 1:Y) {
  p.S2o[y] ~ dbeta(B1.So[1], B2.So[1])
  p.S3o[y] ~ dbeta(B1.So[2], B2.So[2])
  p.S4o[y] ~ dbeta(B1.So[3], B2.So[3]) 
  tele.S2[y, 7] ~  dbinom(p.S2o[y], Ntele.S2[y])  #Eq. 20
  tele.S3[y, 3] ~  dbinom(p.S3o[y], Ntele.S3[y])
  tele.S4[y, 5] ~  dbinom(p.S4o[y], Ntele.S4[y])
}

### AIR SURVEY
#Observability
#index by i since observability and survey errors are modeled hierarchically.
# Theta set up as a glm although I’m not sure a good covariate is accessible.
for (i in 1:I) {b1.theta[i] ~ dnorm(mu_b1t, tau_b1t)}
mu_b1t ~ dnorm(0, 0.0001)
tau_b1t ~ dgamma(0.001,0.001)

for (i in 1:I){
  for (y in 1:Y){
    logit(theta[i, y]) <- b1.theta[i]
    }
  }

# Hierarchical air survey errors
for (i in 1:I){
	sigma.air[i] <- abs(z.air[i]) / sqrt(g.air[i]) 
	z.air[i] ~ dnorm(0, invCsq)
	g.air[i] ~ dgamma(0.5, 0.5)
	tau.air[i] <- 1 / sigma.air[i] / sigma.air[i]
}
C_as ~ dunif(0,1)
invCsq <- 1 / C_as / C_as

#AIR SURVEY DATA
# Deshka
for(y in 1:Y){
	log.t1S1[y] <- log(theta[1, y] * S[y, 1])  #Eq. 22
	air.S1[y] ~ dlnorm(log.t1S1[y], tau.air[1])
	}
# East Susitna, T_s-1=6	
for(t in 1:6) {
	for(y in 1:Y){
	log.tpS2[y, t] <- log(theta[(t + 1), y] * p.S2[y, t] * S[y, 2])  #Eq. 22
	air.S2[y, t] ~ dlnorm(log.tpS2[y, t], tau.air[t + 1])
	}
}
# Talkeetna Survey data
for(t in 1:2) {
	for(y in 1:Y){
	log.tpS3[y, t] <- log(theta[(t + 7), y] * p.S3[y, t] * S[y, 3])  #Eq. 22
	air.S3[y, t] ~ dlnorm(log.tpS3[y, t], tau.air[t + 7])
	}
}
# Yentna Survey data
for(t in 1:4) {
	for(y in 1:Y){
	log.tpS4[y, t] <- log(theta[(t + 9), y] * p.S4[y, t] * S[y, 4])  #Eq. 22
	air.S4[y, t] ~ dlnorm(log.tpS4[y, t], tau.air[t + 9])
	}
}
   
### WEIR COUNTS W (SMALL) LOGNORMAL ERRORS, DETECTABILITY = 1
# tau.weir=400 so cv.weir=0.05
  for (y in 1:Y) {
    log.11S1[y] <- log(IR_deshka[y])
	  weir[y, 1] ~ dlnorm(log.11S1[y], 400)  #Eq. 23 when s=1, Deshka
	log.1p4S2[y] <- log(p.S2[y, 4] * S[y, 2])
      weir[y, 2] ~ dlnorm(log.1p4S2[y], 400)  #Eq. 23 when s=2, Montana
	log.1p6S2[y] <- log(p.S2[y, 6] * S[y, 2])
      weir[y, 3] ~ dlnorm(log.1p6S2[y], 400)  #Eq. 23 when s=2, Willow/Deception
    }

p.HDeshka.mean ~ dbeta(1, 1)
Bscale.HDeshka ~ dunif(0.07, 1)
Bsum.HDeshka <- 1 / Bscale.HDeshka / Bscale.HDeshka
B1.HDeshka <- Bsum.HDeshka * p.HDeshka.mean
B2.HDeshka <- Bsum.HDeshka - B1.HDeshka
# INRIVER RUN AND HARVESTS ESTIMATED
for (y in 1:Y) {
  mu.Hmarine[y] ~ dbeta(0.5,0.5)
  Hmarine[y] <- mu.Hmarine[y] * sum(N[y, ])  #Eq. 8
  logHm[y] <- log(Hmarine[y])
  tau.logHm[y] <- 1 / log(cv.Hm[y]*cv.Hm[y] + 1)
  Hm.hat[y] ~ dlnorm(logHm[y],tau.logHm[y])  #Eq. 19   
  # MR estimates gt 500mm fish, reduce IR to same size class
  p.small3[y] ~ dbeta(1,1)
  p.small4[y] ~ dbeta(1,1)
  small3[y, 1] ~ dbinom(p.small3[y], small3[y, 2])  #Eq. 17
  small4[y, 1] ~ dbinom(p.small4[y], small4[y, 2])
  for (s in 1:SG){
    IR[y, s] <- N[y, s] * (1 - mu.Hmarine[y])  #Eq. 9
	IR500[y, s] <- IR[y, s] * (1 - (q[y, 1] * p.small3[y] + q[y, 2] * p.small4[y]))  #Eq. 18
    logIR500[y, s] <- log(IR500[y, s])
	tau.logMR[y, s] <- 1 / log(cv.MR[y, s]*cv.MR[y, s] + 1)  #Eq. 16
    MR[y, s] ~ dlnorm(logIR500[y, s], tau.logMR[y, s])  #Eq. 15    
	mu.Habove[y, s] ~ dbeta(0.5,0.5)
	Habove[y, s] <- mu.Habove[y, s] * IR[y, s]  #Eq. 10
	logHa[y, s] <- log(Habove[y, s])
	tau.logHa[y, s] <- 1 / log(cv.Ha[y, s]*cv.Ha[y, s] + 1)
	Ha.hat[y, s] ~ dlnorm(logHa[y, s], tau.logHa[y, s])       
	S[y, s] <- max(IR[y, s] - Habove[y, s], 1)  #Eq. 12
  }
  # Harvest upstream of Deshka weir
  p.HDeshka[y] ~ dbeta(B1.HDeshka, B2.HDeshka)
  HDeshka[y] <- p.HDeshka[y] * Habove[y, 1]  #Eq. 11
  logHd[y] <- log(HDeshka[y])
  tau.logHd[y] <- 1 / log(cv.Hd[y]*cv.Hd[y] + 1)
  Hd.hat[y] ~ dlnorm(logHd[y], tau.logHd[y])
  IR_deshka[y] <- S[y, 1] + HDeshka[y]  #Eq. 13
}
} 