################################################################################
#  
#  RJAGS model
#  
################################################################################
model{
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


# PROPORTIONS TO MAINSTEM V YENTNA BY CALENDAR YEAR
  Bfork.scale ~ dunif(0.01,1)
  Bfork.sum <- 1 / (Bfork.scale * Bfork.scale)
  pi.fork.main ~ dbeta(1,1)
  pi.fork.yent <- 1 - pi.fork.main
  B1 <- Bfork.sum * pi.fork.main; B2 <- Bfork.sum - B1;
  for(y in 1:(Y)){                                                    
      pf.main[y] ~ dbeta(B1,B2)
      pf.yentna[y] <- 1 - pf.main[y]
      N.main[y]   <- N[y] * pf.main[y]           # DOES NOT INCLUDE ALEXANDER CK
      N.yentna[y] <- N[y] * pf.yentna[y]
      }

# DIRICHLET DISTRIBUTED SUBSTOCK COMPOSITIONs BY CALENDAR YEAR- MAINSTEM
  Dtrib.scale ~ dunif(0.01,1)
  Dtrib.sum <- 1 / (Dtrib.scale * Dtrib.scale)
  pi.main.1p ~ dbeta(0.17,0.83)T(0.03,)
  pi.main.2p ~ dbeta(0.17,0.66)
  pi.main.3p ~ dbeta(0.17,0.50)
  pi.main.4p ~ dbeta(0.17,0.33)
  pi.main.5p ~ dbeta(0.17,0.17)
  pi.main[1] <- pi.main.1p
  pi.main[2] <- pi.main.2p * (1 - pi.main[1])
  pi.main[3] <- pi.main.3p * (1 - pi.main[1] - pi.main[2])
  pi.main[4] <- pi.main.4p * (1 - pi.main[1] - pi.main[2] - pi.main[3])
  pi.main[5] <- pi.main.5p * (1 - pi.main[1] - pi.main[2] - pi.main[3] - pi.main[4])
  pi.main[6] <- 1 -  pi.main[1] - pi.main[2] - pi.main[3] - pi.main[4] - pi.main[5]
#  pi.main[6] <- pi.main.6p * (1 - pi.main[1] - pi.main[2] - pi.main[3] - pi.main[4] - pi.main[5])
#  pi.main[7] <- 1 -  pi.main[1] - pi.main[2] - pi.main[3] - pi.main[4] - pi.main[5] - pi.main[6]
for (trib in 1:6) {
    gamma.main[trib] <- Dtrib.sum * pi.main[trib]
    for (y in 1:Y) {
      gm[y,trib] ~ dgamma(gamma.main[trib],0.1)
      pm[y,trib] <- gm[y,trib]/sum(gm[y,])
      Nm[y,trib] <- N.main[y] * pm[y,trib]
      }
    }

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
    telemetry[y, 1:6] ~  dmulti(pm[y, ], radios.main[y])
    telemetry[y, 7:11] ~ dmulti(py[y, ], radios.yentna[y])
}
  
# AIR SURVEY DETECTABILITIES BY TRIB
  Btheta.scale ~ dunif(0.01,1)
  Btheta.sum <- 1 / (Btheta.scale * Btheta.scale)
  theta.mean ~ dbeta(1,1)
  Bt1 <- Btheta.sum * theta.mean; Bt2 <- Btheta.sum - Bt1;

# AIR SURVEY COUNTS W LOGNORMAL ERRORS
# px[y,t] ARE FRACTIONS  OF MAIN OR YENTNA RETURNING BY TRIB BY YEAR
# ASSUME THAT HARVEST ABOVE ALEXANDER CK IS PROPORTIONAL TO RUN, FOR NOW 
tau.asmain ~ dgamma(0.01,0.01)
sigma.asmain <- 1 / sqrt(tau.asmain)
for(trib in 1:6) { 
  theta[trib] ~ dbeta(Bt1,Bt2)
  for (y in 1:Y) {
    log.tppS[y,trib] <- log(theta[trib] * pf.main[y] * pm[y,trib] * S[y])
    air.surveys[y,trib] ~ dlnorm(log.tppS[y,trib],tau.asmain)
    }
  }
tau.asyent ~ dgamma(0.01,0.01)
sigma.asyent <- 1 / sqrt(tau.asyent)
for(trib in 7:11) { 
  theta[trib] ~ dbeta(Bt1,Bt2)
  for (y in 1:Y) {
    log.tppS[y,trib] <- log(theta[trib] * pf.yentna[y] * py[y,trib-6] * S[y])
    air.surveys[y,trib] ~ dlnorm(log.tppS[y,trib],tau.asyent)
    }
  }
# DESHKA WEIR COUNTS W (SMALL) LOGNORMAL ERRORS
# DETECTABILITY IS ONE;
# pm[y,2] ARE PROPORTIONS OF MAINSTEM RUN RETURNING TO DESHKA BY YEAR
  tau.weir ~ dgamma(0.1,0.1)
  sigma.weir <- 1 / sqrt(tau.weir)
  for (y in 1:Y) {
    log.p1pS[y] <- log(pm[y,2] * pf.main[y] * S[y])                  # INDEX 2 = DESHKA
    weir.deshka[y] ~ dlnorm(log.p1pS[y],tau.weir)
    }

# INRIVER RUN AND HARVESTS ESTIMATED
for (y in 1:Y) {
  mu.Hmarine[y] ~ dbeta(0.1,0.1)
  H.marine[y] <- mu.Hmarine[y] * N[y]
  log.Hm[y] <- log(H.marine[y])
  tau.log.Hm[y] <- 1 / log(cv.hm[y]*cv.hm[y] + 1)
  Hm.hat[y] ~ dlnorm(log.Hm[y],tau.log.Hm[y])             
  IR[y] <- max(N[y] - H.marine[y], 1)                # IR @ RM 0

  ps34[y] ~ dbeta(1,1)
  s34[y] ~ dbinom(ps34[y], n34[y]) 
  IR.yentna[y] <- N.yentna[y] * (1 - mu.Hmarine[y]) * (1 - q[y, 1] * ps34[y]) 
  IR.main[y]   <- N.main[y]   * (1 - mu.Hmarine[y]) * (1 - q[y, 1] * ps34[y])
  log.IRm[y] <- log(IR.main[y])
  log.IRy[y] <- log(IR.yentna[y])
  tau.log.mrm[y] <- 1 / log(cv.mrm[y]*cv.mrm[y] + 1)
  tau.log.mry[y] <- 1 / log(cv.mry[y]*cv.mry[y] + 1)
  MR.mainstem[y] ~ dlnorm(log.IRm[y],tau.log.mrm[y])    
  MR.yentna[y]   ~ dlnorm(log.IRy[y],tau.log.mry[y])      
  
  mu.Habove[y] ~ dbeta(0.1,0.1)
  H.above[y] <- mu.Habove[y] * IR[y]
  log.Ha[y] <- log(H.above[y])
  tau.log.Ha[y] <- 1 / log(cv.ha[y]*cv.ha[y] + 1)
  Ha.hat[y] ~ dlnorm(log.Ha[y],tau.log.Ha[y])       
  S[y] <- max(IR[y] - H.above[y], 1) 

  mu[y] <- (H.marine[y]              + H.above[y]) / N[y]
  }
} 