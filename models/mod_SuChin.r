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
		log.R.mean1[c, stock] <- log(S.ys[c-a.max, stock]) + lnalpha[stock] - beta[stock] * S.ys[c-a.max, stock] 
		log.resid[c, stock] <- log(R[c, stock]) - log.R.mean1[c, stock]
		lnalpha.y[c, stock] <- lnalpha[stock] + log.resid[c, stock] 
		}
	  log.R.mean2[A+a.min, stock] <- log.R.mean1[A+a.min, stock] + phi * log.resid.0[stock]
	  for (c in (A+a.min+1):(Y+A-1)) {
		log.R.mean2[c, stock] <- log.R.mean1[c, stock] + phi * log.resid[c-1, stock]
		}
	  lnalpha[stock] ~ dnorm(mu_lnalpha, tau_lnalpha)T(0,) #dnorm(0,1.0E-6)T(0,)
	  beta[stock] ~ dnorm(mu_beta, tau_beta)T(0, ) #dnorm(0,1.0E-2)T(0,)                                                             
	  log.resid.0[stock] ~ dnorm(0,tau.red)T(-3,3) 
	  alpha[stock] <- exp(lnalpha[stock])
	  lnalpha.c[stock] <- lnalpha[stock] + (sigma.white * sigma.white / 2 / (1-phi*phi) )
	  S.max[stock] <- 1 / beta[stock]
	  S.eq[stock] <- lnalpha.c[stock] * S.max[stock]
	  S.msy[stock] <- S.eq[stock] * (0.5 - 0.07*lnalpha.c[stock])
	  U.msy[stock] <- lnalpha.c[stock] * (0.5 - 0.07*lnalpha.c[stock])

	# BROOD YEAR RETURNS W/O SR LINK DRAWN FROM COMMON LOGNORMAL DISTN
	  mean.log.R[stock] ~ dnorm(0,1.0E-4)T(0,)       
#	  tau.R[stock] ~ dgamma(0.001,0.001)      
	  R.0[stock] <- exp(mean.log.R[stock])
#	  sigma.R0[stock] <- 1 / sqrt(tau.R[stock])
	  for (c in 1:a.max) { 
		log.R[c, stock] ~ dt(mean.log.R[stock],tau.R,500)   
		R[c, stock] <- exp(log.R[c, stock])
		}
	}
	phi ~ dnorm(0,1.0E-4)T(-1,1)
	mu_lnalpha ~ dnorm(0, 1E-6)T(0,)
	mu_beta ~ dnorm(0, 1E-6)T(0,)
	tau_lnalpha ~ dgamma(0.001,0.001)
	tau_beta ~ dgamma(0.001,0.001)
	tau.white ~ dgamma(0.001,0.001)
	tau.red <- tau.white * (1-phi*phi)
	sigma.white <- 1 / sqrt(tau.white)
	sigma.red <- 1 / sqrt(tau.red)
	tau.R ~ dgamma(0.001,0.001)      
	sigma.R0 <- 1 / sqrt(tau.R)
       
# GENERATE MLD MATURITY SCHEDULES, ONE PER BROOD YEAR
# MULTIVARIATE LOGISTIC MODEL CONTROLS TIME-TREND OF EXPECTED MATURITY
# GIVEN EXPECTED MATURITY, ANNUAL MATURITY SCHEDULES DIRICHLET DISTRIB AT COHORT (BROOD YEAR) c
  D.scale ~ dunif(0.01,1)
  D.sum <- 1 / (D.scale * D.scale)
#  ML1[A] <- 1  
  ML1[A] <- 0  
  ML2[A] <- 0
for (a in 1:(A-1)) { 
  ML1[a] ~ dnorm(0,0.0001) 
  ML2[a] ~ dnorm(0,0.0001) 
  }

for (c in 1:(Y+A-1)) {
  for (a in 1:A) {
    logistic.a[c,a] <- exp(ML1[a] + ML2[a] * c)
    pi.y[c,a] <- logistic.a[c,a] / sum(logistic.a[c,])
    gamma[c,a] <- D.sum * pi.y[c,a]
    g[c,a] ~ dgamma(gamma[c,a],0.1)
    p[c,a] <- g[c,a]/sum(g[c,])
    }
  }

# ASSIGN PRODUCT OF P AND R TO ALL CELLS IN N MATRIX
# c SUBSCRIPT INDEXES BROOD YEAR (COHORT)
# ASSIGN PRODUCT OF P AND R TO ALL CELLS IN N MATRIX
for (stock in 1:5){
    for (a in 1:A) {
        for (c in a:(Y + (a - 1))) {
            N.tas[c - (a - 1), (A + 1 - a), stock] <- p[c, (A + 1 - a)] * R[c, stock]
            }
        }
	}

# CALENDAR YEAR AGE COMPOSITION 
  for (y in 1:Y) {
    N[y] <- sum(N.tas[y,1:A, 1:5])
    for (a in 1:A) {
	  N.ta[y,a] <- sum(N.tas[y,a, 1:5])
	  q[y,a] <- N.ta[y,a] / N[y]
      }
    }
	
  for (y in 1:Y) {
    for (stock in 1:5) {
	    N.ts[y, stock] <- sum(N.tas[y,1:A, stock])
      }
    }
# MULTINOMIAL SCALE SAMPLING ON TOTAL ANNUAL RETURN N
# INDEX t IS CALENDAR YEAR
# Adjustment for sampling program         
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

# MAINSTEM V YENTNA BY CALENDAR YEAR
  for(y in 1:(Y)){                                                    
      N.main[y]   <- sum(N.ts[y, c(1, 2, 3, 5)])            # DOES NOT INCLUDE ALEXANDER CK
      N.yentna[y] <- N.ts[y, 4]
      }
# #PROPORTIONS TO MAINSTEM V YENTNA BY CALENDAR YEAR
  # Bfork.scale ~ dunif(0.01,1)
  # Bfork.sum <- 1 / (Bfork.scale * Bfork.scale)
  # pi.fork.main ~ dbeta(1,1)
  # pi.fork.yent <- 1 - pi.fork.main
  # B1 <- Bfork.sum * pi.fork.main; B2 <- Bfork.sum - B1;
  # for(y in 1:(Y)){                                                    
      # pf.main[y] ~ dbeta(B1,B2)
      # pf.yentna[y] <- 1 - pf.main[y]
      # N.main[y]   <- N[y] * pf.main[y]           # DOES NOT INCLUDE ALEXANDER CK
      # N.yentna[y] <- N[y] * pf.yentna[y]
      # }

# DIRICHLET DISTRIBUTED SUBSTOCK COMPOSITIONs BY CALENDAR YEAR- MAINSTEM
  Dtrib.scale ~ dunif(0.01,1)
  Dtrib.sum <- 1 / (Dtrib.scale * Dtrib.scale)
  pi.main.1p ~ dbeta(0.17,0.83)T(0.03,)
  pi.main.2p ~ dbeta(0.17,0.66)
  pi.main.3p ~ dbeta(0.17,0.50)

  pi.main[1] <- pi.main.1p
  pi.main[2] <- pi.main.2p * (1 - pi.main[1])
  pi.main[3] <- 1 -  pi.main[1] - pi.main[2]
for (trib in 1:3) {
    gamma.main[trib] <- Dtrib.sum * pi.main[trib]
    for (y in 1:Y) {
      gm[y,trib] ~ dgamma(gamma.main[trib],0.1)
      pm[y,trib] <- gm[y,trib]/sum(gm[y,])
      Nm[y,trib] <- N.ts[y, 5] * pm[y,trib]
      }
    }
# # DIRICHLET DISTRIBUTED SUBSTOCK COMPOSITIONs BY CALENDAR YEAR- MAINSTEM
  # Dtrib.scale ~ dunif(0.01,1)
  # Dtrib.sum <- 1 / (Dtrib.scale * Dtrib.scale)
  # pi.main.1p ~ dbeta(0.17,0.83)T(0.03,)
  # pi.main.2p ~ dbeta(0.17,0.66)
  # pi.main.3p ~ dbeta(0.17,0.50)
  # pi.main.4p ~ dbeta(0.17,0.33)
  # pi.main.5p ~ dbeta(0.17,0.17)
  # pi.main[1] <- pi.main.1p
  # pi.main[2] <- pi.main.2p * (1 - pi.main[1])
  # pi.main[3] <- pi.main.3p * (1 - pi.main[1] - pi.main[2])
  # pi.main[4] <- pi.main.4p * (1 - pi.main[1] - pi.main[2] - pi.main[3])
  # pi.main[5] <- pi.main.5p * (1 - pi.main[1] - pi.main[2] - pi.main[3] - pi.main[4])
  # pi.main[6] <- 1 -  pi.main[1] - pi.main[2] - pi.main[3] - pi.main[4] - pi.main[5]
# for (trib in 1:6) {
    # gamma.main[trib] <- Dtrib.sum * pi.main[trib]
    # for (y in 1:Y) {
      # gm[y,trib] ~ dgamma(gamma.main[trib],0.1)
      # pm[y,trib] <- gm[y,trib]/sum(gm[y,])
      # Nm[y,trib] <- N.main[y] * pm[y,trib]
      # }
    # }

# SUBSTOCK COMPOSITIONS- YENTNA
  pi.yent[1] ~ dbeta(0.2,0.8)
  pi.yent.2p ~ dbeta(0.2,0.6)
  pi.yent.3p ~ dbeta(0.2,0.4)
  pi.yent.4p ~ dbeta(0.2,0.2)
  pi.yent[2] <- pi.yent.2p * (1 - pi.yent[1])
  pi.yent[3] <- pi.yent.3p * (1 - pi.yent[1] - pi.yent[2])
  pi.yent[4] <- pi.yent.4p * (1 - pi.yent[1] - pi.yent[2] - pi.yent[3])
  pi.yent[5] <-               1 - pi.yent[1] - pi.yent[2] - pi.yent[3] - pi.yent[4]
for (trib in 1:5) {
    gamma.yent[trib] <- Dtrib.sum * pi.yent[trib]
    for (y in 1:Y) {
      gy[y,trib] ~ dgamma(gamma.yent[trib],0.1)
      py[y,trib] <- gy[y,trib]/sum(gy[y,])
      Ny[y,trib] <- N.yentna[y] * py[y,trib]
      }
    }

# MULTINOMIAL COUNTS OF RADIOS TRACKED TO INDIVIDUAL TRIBS
for (y in 1:Y) { 
    telemetry[y, 1:3] ~  dmulti(pm[y, ], radios.main[y])
    telemetry[y, 4:8] ~ dmulti(py[y, ], radios.yentna[y])
}

# AIR SURVEY COUNTS W LOGNORMAL ERRORS
# px[y,t] ARE FRACTIONS  OF MAIN OR YENTNA RETURNING BY TRIB BY YEAR
# ASSUME THAT HARVEST ABOVE ALEXANDER CK IS PROPORTIONAL TO RUN, FOR NOW 
#theta1 ~ dunif(2, 18)
#theta2 ~ dunif(2, 18)
theta.mean ~ dunif(0.25, 0.75)#dbeta(1, 1)
Btheta.scale ~ dunif(0.01,1)
Btheta.sum <- 1 / (Btheta.scale * Btheta.scale)
for(trib in 1:11) {
#  theta.mean[trib] ~ dbeta(theta1,theta2)#dunif(0.1, 0.9)
  #Btheta.scale[trib] ~ dunif(0.01,1)
  #Btheta.sum[trib] <- 1 / (Btheta.scale[trib] * Btheta.scale[trib])
  Bt1[trib] <- Btheta.sum * theta.mean;#Btheta.sum * theta.mean[trib]; 
  Bt2[trib] <- Btheta.sum - Bt1[trib];
  theta[trib] ~ dbeta(Bt1[trib],Bt2[trib])
#  for (y in 1:Y) {
#	theta[y, trib] ~ dbeta(Bt1[trib],Bt2[trib])
#   }
  }
tau.as ~ dgamma(0.1,0.01)
sigma.as <- 1 / sqrt(tau.as)  
for (y in 1:Y) {
  for (trib in 1:11){
	log.tppS[y, trib] <- log(theta[trib] * S.yt[y, trib]) 
	air.surveys[y, trib] ~ dlnorm(log.tppS[y, trib], tau.as)
  }	
}
   
# DESHKA WEIR COUNTS W (SMALL) LOGNORMAL ERRORS
# DETECTABILITY IS ONE;
# pm[y,2] ARE PROPORTIONS OF MAINSTEM RUN RETURNING TO DESHKA BY YEAR
  tau.weir ~ dgamma(0.01,0.1)
  sigma.weir <- 1 / sqrt(tau.weir)
  for (y in 1:Y) {
    log.p1pS[y] <- log(S.yt[y, 2])                  # INDEX 2 = DESHKA
    weir.deshka[y] ~ dlnorm(log.p1pS[y], tau.weir)
    }

# INRIVER RUN AND HARVESTS ESTIMATED
for (y in 1:Y) {
  mu.Hmarine[y] ~ dbeta(0.5,0.5)
  H.marine[y] <- mu.Hmarine[y] * N[y]
  log.Hm[y] <- log(H.marine[y])
  tau.log.Hm[y] <- 1 / log(cv.hm[y]*cv.hm[y] + 1)
  Hm.hat[y] ~ dlnorm(log.Hm[y],tau.log.Hm[y])             
  IR[y] <- max(N[y] - H.marine[y], 1)                # IR @ RM 0

  ps3[y] ~ dbeta(1,1)
  ps4[y] ~ dbeta(1,1)
  s3[y, 1] ~ dbinom(ps3[y], s3[y, 2])
  s4[y, 1] ~ dbinom(ps4[y], s4[y, 2])
  IR.yentna[y] <- N.yentna[y] * (1 - mu.Hmarine[y]) * (1 - q[y, 1] * ps3[y]) * (1 - q[y, 2] * ps4[y]) 
  IR.main[y]   <- N.main[y]   * (1 - mu.Hmarine[y]) * (1 - q[y, 1] * ps3[y]) * (1 - q[y, 2] * ps4[y])
  log.IRm[y] <- log(IR.main[y])
  log.IRy[y] <- log(IR.yentna[y])
  tau.log.mrm[y] <- 1 / log(cv.mrm[y]*cv.mrm[y] + 1)
  tau.log.mry[y] <- 1 / log(cv.mry[y]*cv.mry[y] + 1)
  MR.mainstem[y] ~ dlnorm(log.IRm[y],tau.log.mrm[y])    
  MR.yentna[y]   ~ dlnorm(log.IRy[y],tau.log.mry[y])

  tau.log.Ha[y] <- 1 / log(cv.ha[y]*cv.ha[y] + 1)
  IR.yt[y, 1] <- Nm[y, 1] * (1 - mu.Hmarine[y])
  IR.yt[y, 2] <- N.ts[y, 1] * (1 - mu.Hmarine[y])
  IR.yt[y, 3] <- N.ts[y, 2] * (1 - mu.Hmarine[y])
  IR.yt[y, 4] <- N.ts[y, 3] * (1 - mu.Hmarine[y])
  IR.yt[y, 5] <- Nm[y, 2] * (1 - mu.Hmarine[y])
  IR.yt[y, 6] <- Nm[y, 3] * (1 - mu.Hmarine[y])
#  for (trib in 1:6){IR.yt[y, trib] <- Nm[y, trib] * (1 - mu.Hmarine[y])}
  for (trib in 1:5){IR.yt[y, trib + 6] <- Ny[y, trib] * (1 - mu.Hmarine[y])}
  for (trib in 1:11){
	mu.Habove[y, trib] ~ dbeta(0.5,0.5)
	H.above[y, trib] <- mu.Habove[y, trib] * IR.yt[y, trib]
	log.Ha[y, trib] <- log(H.above[y, trib])
	Ha.hat[y, trib] ~ dlnorm(log.Ha[y, trib], tau.log.Ha[y])       
	S.yt[y, trib] <- max(IR.yt[y, trib] - H.above[y, trib], 1)
    }
  S[y] <- sum(S.yt[y, ])
  S.ys[y, 1] <- S.yt[y, 2]
  S.ys[y, 2] <- S.yt[y, 3]
  S.ys[y, 3] <- S.yt[y, 4]
  S.ys[y, 4] <- sum(S.yt[y, 7:11])
  S.ys[y, 5] <- sum(S.yt[y, c(1, 5, 6)])
  }
} 