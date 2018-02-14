
model {
  for (c in (A+a.min):(Y+A-1)) {
    log.R[c] ~ dt(log.R.mean2[c],tau.white,500)
    R[c] <- exp(log.R[c])
    log.R.mean1[c] <- log(S[c-a.max]) + lnalpha - beta * S[c-a.max] 
    log.resid[c] <- log(R[c]) - log.R.mean1[c]
    lnalpha.y[c] <- lnalpha + log.resid[c] 
  }
  log.resid.vec <- log.resid[(A+a.min):(Y+A-1)]
  lnalpha.vec <- lnalpha.y[(A+a.min):(Y+A-1)]
  log.R.mean2[A+a.min] <- log.R.mean1[A+a.min] + phi * log.resid.0
  for (c in (A+a.min+1):(Y+A-1)) {
    log.R.mean2[c] <- log.R.mean1[c] + phi * log.resid[c-1]
  }
  lnalpha ~ dnorm(0,1.0E-6)T(0,)
  beta ~ dnorm(0,1.0E-2)T(0,)              
  phi ~ dnorm(0,1.0E-4)T(-1,1)                                       
  tau.white ~ dgamma(0.001,0.001)        
  log.resid.0 ~ dnorm(0,tau.red)T(-3,3) 
  alpha <- exp(lnalpha)
  tau.red <- tau.white * (1-phi*phi)
  sigma.white <- 1 / sqrt(tau.white)
  sigma.red <- 1 / sqrt(tau.red)
  lnalpha.c <- lnalpha + (sigma.white * sigma.white / 2 / (1-phi*phi) )
  S.max <- 1 / beta
  S.eq <- lnalpha.c * S.max
  S.msy <- S.eq * (0.5 - 0.07*lnalpha.c)
  U.msy <- lnalpha.c * (0.5 - 0.07*lnalpha.c)
  
  # BROOD YEAR RETURNS W/O SR LINK DRAWN FROM COMMON LOGNORMAL DISTN
  mean.log.R ~ dnorm(0,1.0E-4)T(0,)       
  tau.R ~ dgamma(0.001,0.001)      
  R.0 <- exp(mean.log.R)
  sigma.R0 <- 1 / sqrt(tau.R)
  for (c in 1:a.max) { 
    log.R[c] ~ dt(mean.log.R,tau.R,500)   
    R[c] <- exp(log.R[c])
  }
  
  # GENERATE MLD MATURITY SCHEDULES, ONE PER BROOD YEAR
  # MULTIVARIATE LOGISTIC MODEL CONTROLS TIME-TREND OF EXPECTED MATURITY
  # GIVEN EXPECTED MATURITY, ANNUAL MATURITY SCHEDULES DIRICHLET DISTRIB AT COHORT (BROOD YEAR) c
  D.scale ~ dunif(0,1)
  D.sum <- 1 / (D.scale * D.scale)
  ML1[A] <- 1  
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
  for (a in 1:A) {
    for (c in a:(Y + (a - 1))) {
      N.ta[c - (a - 1), (A + 1 - a)] <- p[c, (A + 1 - a)] * R[c]
    }
  }
  
  # CALENDAR YEAR AGE COMPOSITION 
  for (y in 1:Y) {
    N[y] <- sum(N.ta[y,1:A])
    for (a in 1:A) {
      q[y,a] <- N.ta[y,a] / N[y]
    }
  }
  # MULTINOMIAL SCALE SAMPLING ON TOTAL ANNUAL RETURN N
  # INDEX t IS CALENDAR YEAR
  # OVERLAP IS MUCH LARGER THAN IN PREVIOUS VERSIONS         
  for (y in 1:Y) {  
    x.a[y, 1:A] ~ dmulti(q[y, ], n.a[y])
  }
  
  # GENERATE SUBSTOCK COMPOSITIONS, ONE PER CALENDAR YEAR
  # ANNUAL SUBSTOCK COMPOSITIONS DIRICHLET DISTRIBUTED
  Dtrib.scale ~ dunif(0,1)
  Dtrib.sum <- 1 / (Dtrib.scale * Dtrib.scale)
  pi.trib[1] ~ dbeta(0.2,0.8)
  pi.trib.2p ~ dbeta(0.25,0.75)
  pi.trib.3p ~ dbeta(0.33,0.67)
  pi.trib.4p ~ dbeta(0.5,0.5)
  pi.trib[2] <- pi.trib.2p * (1 - pi.trib[1])
  pi.trib[3] <- pi.trib.2p * (1 - pi.trib[1] - pi.trib[2])
  pi.trib[4] <- pi.trib.2p * (1 - pi.trib[1] - pi.trib[2] - pi.trib[3])
  pi.trib[5] <-            1 - pi.trib[1] - pi.trib[2] - pi.trib[3] - pi.trib[4]
  for (trib in 1:5) {
    gamma.trib[trib] <- Dtrib.sum * pi.trib[trib]
    for (y in 1:Y) {
      gt[y,trib] ~ dgamma(gamma.trib[trib],0.1)
      pt[y,trib] <- gt[y,trib]/sum(gt[y,])
    }
  }
  
  # MULTINOMIAL COUNTS OF RADIOS TRACKED TO INDIVIDUAL TRIBS
  for (y in 1:Y) {  
    #  radios[y] <- sum(telemetry[y,])
    telemetry[y, 1:A] ~ dmulti(pt[y, ], radios[y])
  }
  
  # INRIVER RUN AND HARVESTS ESTIMATED
  for (y in 1:Y) {
    mu.Hbelow[y] ~ dbeta(0.1,0.1)
    H.below[y] <- mu.Hbelow[y] * N[y]
    log.Hb[y] <- log(H.below[y])
    tau.log.Hb[y] <- 1 / log(cv.hb[y]*cv.hb[y] + 1)
    Hb.hat[y] ~ dlnorm(log.Hb[y],tau.log.Hb[y])             
    
    InriverRun[y] <- max(N[y] - H.below[y], 1)
    log.IR[y] <- log(InriverRun[y])
    tau.log.mr[y] <- 1 / log(cv.mr[y]*cv.mr[y] + 1)
    MR[y] ~ dlnorm(log.IR[y],tau.log.mr[y])         # MARK RECAP 
    
    mu.Habove[y] ~ dbeta(0.1,0.1)
    H.above[y] <- mu.Habove[y] * InriverRun[y]
    log.Ha[y] <- log(H.above[y])
    tau.log.Ha[y] <- 1 / log(cv.ha[y]*cv.ha[y] + 1)
    Ha.hat[y] ~ dlnorm(log.Ha[y],tau.log.Ha[y])       
    mu[y] <- (H.below[y] + H.above[y]) / N[y]
    S[y] <- max(InriverRun[y] - H.above[y], 1)
    log.S[y] <- log(S[y])
  }
  
  # AIR SURVEY COUNTS W LOGNORMAL ERRORS
  # theta ARE AIR SURVEY DETECTABILITIES BY TRIB;
  # pt[y,t] ARE FRACTIONS RETURNING BY TRIB BY YEAR
  for(trib in 1:4) { 
    theta[trib] ~ dbeta(0.5,0.5)
    tau.trib[trib] ~ dgamma(0.01,0.01)
    sigma.trib[trib] <- 1 / sqrt(tau.trib[trib])
    for (y in 1:Y) {
      log.rpS[y,trib] <- log(theta[trib] * pt[y,trib] * S[y])
      air.surveys[y,trib] ~ dlnorm(log.rpS[y,trib],tau.trib[trib])
    }
  }
  # DESHKA WEIR COUNTS W (SMALL) LOGNORMAL ERRORS
  # DETECTABILITY IS ONE;
  # pt[y,1] ARE FRACTIONS RETURNING TO DESHKA BY YEAR
  tau.weir ~ dgamma(0.01,0.01)
  sigma.weir <- 1 / sqrt(tau.weir)
  for (y in 1:Y) {
    log.pS[y] <- log(pt[y,1] * S[y])
    weir.deshka[y] ~ dlnorm(log.pS[y],tau.weir)
  }
  
}