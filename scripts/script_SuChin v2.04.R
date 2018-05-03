################################################################

####  v0.01 simdat
   #  R code from YukCanChin 7.06 
   #  TAM trending age at maturity
   #  allocation among tributaries dirichlet distributed
   #  data from Susitna Chinook simdata ddmmmyy.xlsx
   #
####  v1.01 hierarchical     FAIL
   #  Feb 2018 Susitna data
   #  Theta hierarchical to provide inference about Alexander Creek air counts
   #  Error in node mu.Halex[5]  Slicer stuck at value with infinite density
   #  
####  v1.02 fake AlexCk weir data  FAIL
   #  Added 3 years of data from a fake weir on Alex Ck
   #  Error in node mu.Halex[21]  Slicer stuck at value with infinite density
   #  
####  v1.03 omit AlexCk from model
   #  p.main[1] <<< 1% - should be ~5%
   #  note that harvest still contains some Alex Ck fish
   #  
####  v1.04 
   #  truncating pi.main[1]T(0.03,) solved mixing problem
   #  telemetry data are wrong (faked)
   #  
####  v2.01 
   #  complete telemetry data from David 23 Feb
   #  MR CVs too large
   #  
####  v2.02 
   #  
   #  better MR CVs
   #  
####  v2.03 forecast for Tim
   #  
####  v2.04 
   #  
   #  ML1[A]=0
   # 

packs <- c("SusitnaEG", "rjags", "coda")
lapply(packs, require, character.only = TRUE)


#options(digits=8)        ## set output digits
#options(scipen=5)        ## reduce scientific notation   

####   Start here to create new version  #######################################################

rm(list=ls(all=TRUE))

stock<-"SuChin"
version <- "v2_04"

year <- 1979:2017
nyrs=as.numeric(length(year))
fyr=min(year)
lyr=max(year)

Hm.hat <- c(Hm$Hm_Susitna, NA)
cv.hm <- rep(0.05, length(Hm.hat))

MR.yentna <- mr$mr_yentna
cv.mr.y  <-  mr$cv_yentna                   
MR.mainstem <- mr$mr_mainstem
cv.mr.m  <-  mr$cv_mainstem

weir.deshka <- weir[grepl("Deshka", weir$trib), "count"] %>% unlist()
cv.wd <- rep(0.05, length(weir.deshka)) 

Ha.hat <-
  Ha[, -which(colnames(Ha) %in% c("A", "year"))] %>%
  apply(1, sum, na.rm = TRUE) %>%
  c(., rep(NA, 2))
cv.ha <- rep(0.2, length(Ha.hat))
 
#age comp count data
x.a <- 
  age[grepl("Deshka", age$location), ] %>%
  dplyr::mutate(x34 = x3 + x4,
                x678 = x6 + x78) %>%
  dplyr::select(x34, x5, x678) %>%
  as.matrix()
n.a <- age[grepl("Deshka", age$location), "n"] %>% ifelse(is.na(.), 100, .) 
nages <- ncol(x.a)
amin=4
amax=6

air.surveys <- as_complete[, !grepl("year|A", colnames(as_complete))] %>% as.matrix()

telemetry <- as.matrix(telemetry)
radios.main <- rowSums(telemetry[,1:6])                                       
radios.yentna <- rowSums(telemetry[,7:11]) 



####  Bundle data to be passed to JAGS  ####
dat=list(Y = nyrs, A=nages, a.min=amin, a.max=amax,
 x.a=x.a, n.a=n.a, air.surveys=air.surveys, 
 telemetry=telemetry, radios.main=radios.main,radios.yentna=radios.yentna,
 Hm.hat=Hm.hat, cv.hm=cv.hm,
 Ha.hat=Ha.hat, cv.ha=cv.ha,
 MR.mainstem=MR.mainstem, cv.mrm=cv.mr.m,
 MR.yentna=MR.yentna,     cv.mry=cv.mr.y,
 weir.deshka=weir.deshka
 )

B.scale.init = 0.25
D.scale.init = 0.17
lnalpha.init = 1.6
beta.init = 0.00002
log.resid.0.init = 0
mean.log.R.init = 11.3
phi.init = 0.49
pi.main.1.init = 0.1
pi.main.2p.init = 0.3
pi.main.3p.init = 0.5
pi.main.4p.init = 0.5
pi.main.5p.init = 0.5
pi.yent.1.init = 0.1
pi.yent.2p.init = 0.3
pi.yent.3p.init = 0.5
pi.yent.4p.init = 0.5
tau.R.init = 1.93
tau.white.init = 16.65
tau.init = 5
ML1.inits = c(-1,0,NA) 
ML2.inits = c(0.05,0,NA) 

####  initial parameter values to be passed to JAGS  ####

inits1=list(
pi.main.1p=pi.main.1.init,
Bfork.scale=B.scale.init,
Btheta.scale=B.scale.init,
D.scale=D.scale.init,
Dtrib.scale=D.scale.init,
beta=beta.init,
lnalpha=lnalpha.init,
log.resid.0=log.resid.0.init,
mean.log.R=mean.log.R.init,
phi=phi.init,
tau.R=tau.R.init,
tau.white=tau.white.init,
tau.asmain=tau.init,
tau.asyent=tau.init,
tau.weir=tau.init,
ML1=ML1.inits,
ML2=ML2.inits
)

# bundle inits for JAGS
inits=list(inits1,inits1)

####        Define the parameters (nodes) of interest   ##### 
parameters=c(
'beta','sigma.white','sigma.R0',
'lnalpha','lnalpha.c','alpha','lnalpha.vec', 
'phi','log.resid.0','log.resid.vec',
'S.eq','S.max','S.msy','U.msy',
'pi.y','D.sum','D.scale','ML1','ML2',
'mu','mu.Hmarine','mu.Habove',
'S','N','R','IR','IR.main','IR.yentna',
'p','N.ta','q',
'Bfork.sum','Dtrib.sum','Btheta.sum','Btheta.scale',
'pi.fork.main','pi.fork.yent','pf.main','pf.yentna',
'pi.main','pi.yent','pm','py',
'theta','sigma.asmain','sigma.asyent','sigma.weir'
)

#### run JAGS ####
ptm = proc.time()
jmod = jags.model(file=".\\models\\mod_SuChin v2_04.r", data=dat, n.chains=2, inits=inits, n.adapt=1000)  
#update(jmod, n.iter=1000, by=1, progress.bar='text')               
#post = coda.samples(jmod, parameters, n.iter=10000, thin=1)        # 10 min
#update(jmod, n.iter=2000, by=1, progress.bar='text')               
#post = coda.samples(jmod, parameters, n.iter=20000, thin=10)         
update(jmod, n.iter=100000, by=1, progress.bar='text')               
post = coda.samples(jmod, parameters, n.iter=100000, thin=50)         #  1.5h
#post = coda.samples(jmod, parameters, n.iter=600000, thin=300)       #  
endtime = proc.time()-ptm
endtime[3]/60/60  

#load(file=paste(stock,version,"post") ) 
#saveRDS(post, file = paste0(".\\posts\\", stock, version, ".rds"))
post <- readRDS(paste0(".\\posts\\", stock, version, ".rds"))

#inspect convergence
shinystan::launch_shinystan(shinystan::as.shinystan(post))

summary <- get_summary(post)

#2d age at maturity and age comp arrays
table_age(summary, "p") #age-at-maturity
table_age(summary, "q") #age comp
table_age(summary, "N.ta") #total run by age

plot_age(as.data.frame(x.a), summary)

table_stock(summary)
plot_stock(telemetry, summary)

plot_state(summary)

plot_statepairs(post)
plot_horse(post, summary, 325000)
plot_profile(get_profile(post, 200000))
plot_ey(get_profile(post, 125000), plot_max = 125000)

#### Collect posterior stats on scalars and vectors #####################################

      pi.fork.main.md     =statsquants[substr(rownames(statsquants),1,12)=="pi.fork.main",7]
      pi.fork.yent.md     =statsquants[substr(rownames(statsquants),1,12)=="pi.fork.yent",7]

      pi.maindown.md      =statsquants[substr(rownames(statsquants),1,10)=="pi.main[1]",7]
      pi.deshka.md        =statsquants[substr(rownames(statsquants),1,10)=="pi.main[2]",7]
      pi.east.md          =statsquants[substr(rownames(statsquants),1,10)=="pi.main[3]",7]
      pi.talkeetna.md     =statsquants[substr(rownames(statsquants),1,10)=="pi.main[4]",7]
      pi.mainup.md        =statsquants[substr(rownames(statsquants),1,10)=="pi.main[5]",7]
      pi.chulitna.md      =statsquants[substr(rownames(statsquants),1,10)=="pi.main[6]",7]
      pi.lake.md          =statsquants[substr(rownames(statsquants),1,10)=="pi.yent[1]",7]
      pi.kahiltna.md      =statsquants[substr(rownames(statsquants),1,10)=="pi.yent[2]",7]
      pi.talachulitna.md  =statsquants[substr(rownames(statsquants),1,10)=="pi.yent[3]",7]
      pi.skwentna.md      =statsquants[substr(rownames(statsquants),1,10)=="pi.yent[4]",7]
      pi.other.yentna.md  =statsquants[substr(rownames(statsquants),1,10)=="pi.yent[5]",7]

      theta.maindown.md      =statsquants[substr(rownames(statsquants),1,8)=="theta[1]",7]
      theta.deshka.md        =statsquants[substr(rownames(statsquants),1,8)=="theta[2]",7]
      theta.east.md          =statsquants[substr(rownames(statsquants),1,8)=="theta[3]",7]
      theta.talkeetna.md     =statsquants[substr(rownames(statsquants),1,8)=="theta[4]",7]
      theta.mainup.md        =statsquants[substr(rownames(statsquants),1,8)=="theta[5]",7]
      theta.chulitna.md      =statsquants[substr(rownames(statsquants),1,8)=="theta[6]",7]
      theta.lake.md          =statsquants[substr(rownames(statsquants),1,8)=="theta[7]",7]
      theta.kahiltna.md      =statsquants[substr(rownames(statsquants),1,8)=="theta[8]",7]
      theta.talachulitna.md  =statsquants[substr(rownames(statsquants),1,8)=="theta[9]",7]
      theta.skwentna.md      =statsquants[substr(rownames(statsquants),1,8)=="theta[10]",7]
      theta.other.yentna.md  =statsquants[substr(rownames(statsquants),1,8)=="theta[11]",7]

      expanded.deshkaweir   <- weir.deshka     / pi.deshka.md    / pi.fork.main.md                     
      expanded.deshka       <- air.surveys[,2] / pi.deshka.md    / pi.fork.main.md / theta.deshka.md                        
      expanded.east         <- air.surveys[,3] / pi.east.md      / pi.fork.main.md / theta.east.md                        
      expanded.talkeetna    <- air.surveys[,4] / pi.talkeetna.md / pi.fork.main.md / theta.talkeetna.md                        
      expanded.mainup       <- air.surveys[,5] / pi.mainup.md    / pi.fork.main.md / theta.mainup.md                        
      expanded.chulitna     <- air.surveys[,6] / pi.chulitna.md  / pi.fork.main.md / theta.chulitna.md                        
      expanded.lake         <- air.surveys[,7] / pi.lake.md      / pi.fork.yent.md / theta.lake.md                        
      expanded.kahiltna     <- air.surveys[,8] / pi.kahiltna.md  / pi.fork.yent.md / theta.kahiltna.md                        
      expanded.talachulitna <- air.surveys[,9] / pi.talachulitna.md / pi.fork.yent.md / theta.talachulitna.md                        

      lnsig.mrm <- sqrt(log(cv.mrm*cv.mrm + 1))
      lb95.mrm <- exp(log(MR.mainstem)-1.96*lnsig.mrm)
      ub95.mrm <- exp(log(MR.yentna)+1.96*lnsig.mrm)
      lnsig.mry <- sqrt(log(cv.mry*cv.mry + 1))
      lb95.mry <- exp(log(MR.mainstem)-1.96*lnsig.mry)
      ub95.mry <- exp(log(MR.mainstem)+1.96*lnsig.mry)
      
      IR.mn=stats[substr(rownames(stats),1,3)=="IR[",1]; 
      IR.02=quants[substr(rownames(quants),1,3)=="IR[",1]
      IR.98=quants[substr(rownames(quants),1,3)=="IR[",5]

      IRmain.mn=stats[substr(rownames(stats),1,8)=="IR.main[",1]; 
      IRm.02=quants[substr(rownames(quants),1,8)=="IR.main[",1]
      IRm.98=quants[substr(rownames(quants),1,8)=="IR.main[",5]
      
      IRyent.mn=stats[substr(rownames(stats),1,10)=="IR.yentna[",1]; 
      IRy.02=quants[substr(rownames(quants),1,10)=="IR.yentna[",1]
      IRy.98=quants[substr(rownames(quants),1,10)=="IR.yentna[",5]

      TR.mn=stats[substr(rownames(stats),1,2)  =="N[",1]; TR.mn
      TR.02=quants[substr(rownames(quants),1,2)=="N[",1]
      TR.98=quants[substr(rownames(quants),1,2)=="N[",5]

      S.mn=stats[substr(rownames(stats),1,2)=="S[",1]; S.mn
      S.md=quants[substr(rownames(quants),1,2)=="S[",3]; S.md
      S.02=quants[substr(rownames(quants),1,2)=="S[",1]
      S.98=quants[substr(rownames(quants),1,2)=="S[",5]

      R.mn=stats[substr(rownames(stats),1,2)=="R[",1]; R.mn
      R.md=quants[substr(rownames(quants),1,2)=="R[",3]; R.md
      R.02=quants[substr(rownames(quants),1,2)=="R[",1]
      R.98=quants[substr(rownames(quants),1,2)=="R[",5]

      resid.mn=stats[substr(rownames(stats),1,14)=="log.resid.vec[",1]; resid.mn
      resid.02=quants[substr(rownames(quants),1,14)=="log.resid.vec[",1]; 
      resid.98=quants[substr(rownames(quants),1,14)=="log.resid.vec[",5]; 

      mu.mn=stats[substr(rownames(stats),1,3)=="mu[",1]; mu.mn
      mu.02=quants[substr(rownames(quants),1,3)=="mu[",1]
      mu.98=quants[substr(rownames(quants),1,3)=="mu[",5]

      Smsy.50.vec=quants[substr(rownames(quants),1,5)=="S.msy",3]; Smsy.50.vec
      Smsy.50 <- Smsy.50.vec[1]
      Smsy.02.vec=quants[substr(rownames(quants),1,5)=="S.msy",1]; Smsy.02.vec
      Smsy.02 <- Smsy.02.vec[1]
      Smsy.98.vec=quants[substr(rownames(quants),1,5)=="S.msy",5]; Smsy.98.vec
      Smsy.98 <- Smsy.98.vec[1]

      Seq.50.vec=quants[substr(rownames(quants),1,4)=="S.eq",3]; Seq.50.vec
      Seq.50 <- Seq.50.vec[1]

      lna.50.vec=quants[substr(rownames(quants),1,7)=="lnalpha",3]; lna.50.vec[1]
      Umax.50=1-1/exp(lna.50.vec[1]); Umax.50
      str(Umax.50)

      Umsy.50.vec=quants[substr(rownames(quants),1,5)=="U.msy",3]; Umsy.50.vec
      Umsy.50 <- Umsy.50.vec[1]

      Smax.50=quants[substr(rownames(quants),1,5)=="S.max",3]; Smax.50

      SY.mn=stats[substr(rownames(stats),1,3)=="SY[",1]; S.mn
      SY.50=quants[substr(rownames(quants),1,3)=="SY[",3]
      SY.02=quants[substr(rownames(quants),1,3)=="SY[",1]
      SY.98=quants[substr(rownames(quants),1,3)=="SY[",5]


################################################################################
#  
#  Model Fit Plots
#  
################################################################################
#png(paste(stock,version,"FitPanel.png"),width=6.5,height=8.5,units="in",res=1200)
par(mfrow=c(3,1),mar=c(1,4,4,1)+0.1,ps=12)
ylim1 <- max(expanded.deshkaweir[!is.na(expanded.deshkaweir)],expanded.deshka[!is.na(expanded.deshka)])

      plot(S.mn~year, type='b', col="black", pch=15, ps=2, 
           xlim=c(1978,2018), ylim=c(0, ylim1), 
           xaxt='n', xlab="",
           ylab="Escapement")
      lines(S.02~year, col="grey", lty=2) 
      lines(S.98~year, col="grey", lty=2)
      points(expanded.deshkaweir~year, type='p', col="red", pch=18) 
      points(expanded.deshka~year, type='p', col="green", pch=19) 
      axis(side=1)
      legend(x="topright",c("Deshka Weir","Deshka"),pch=c(18,19),
             col=c("red","green"),bty="n",text.col=c("red","green"))

ylim2 <- max(expanded.mainup[!is.na(expanded.mainup)],expanded.talkeetna[!is.na(expanded.talkeetna)],
             expanded.east[!is.na(expanded.east)],expanded.chulitna[!is.na(expanded.chulitna)])

      plot(S.mn~year, type='b', col="black", pch=15, ps=2, 
           xlim=c(1978,2018), ylim=c(0, ylim2), xaxt='n', xlab="",
           ylab="Escapement")
      lines(S.02~year, col="grey", lty=2) 
      lines(S.98~year, col="grey", lty=2)
      points(expanded.east~year, type='p', col="red", pch=18) 
      points(expanded.talkeetna~year, type='p', col="green", pch=19) 
      points(expanded.mainup~year, type='p', col="purple", pch=20) 
      points(expanded.chulitna~year, type='p', col="pink", pch=21) 
      axis(side=1)
      legend(x="topright",c("East","Talkeetna","Main-Upstr","Chulitna"),pch=c(18,19,20,21),
             col=c("red","green","purple","pink"),bty="n",text.col=c("red","green","purple","pink"))

ylim3 <- max(expanded.lake[!is.na(expanded.lake)],expanded.kahiltna[!is.na(expanded.kahiltna)],
             expanded.talachulitna[!is.na(expanded.talachulitna)])

      plot(S.mn~year, type='b', col="black", pch=15, ps=2, 
           xlim=c(1978,2018), ylim=c(0, ylim3), xaxt='n', xlab="",
           ylab="Escapement")
      lines(S.02~year, col="grey", lty=2) 
      lines(S.98~year, col="grey", lty=2)
      points(expanded.lake~year, type='p', col="red", pch=18) 
      points(expanded.kahiltna~year, type='p', col="green", pch=19) 
      points(expanded.talachulitna~year, type='p', col="purple", pch=20) 
      axis(side=1)
      legend(x="topright",c("Lake","Kahiltna","Talachulitna"),pch=c(18,19,20),
             col=c("red","green","purple"),bty="n",text.col=c("red","green","purple"))


      plot(IR.mn~year, type='b', col="black", pch=15, ps=2, 
           xlim=c(1980,2020), ylim=c(0, 250000), xaxt='n', xlab="",
           ylab="Inriver Run")
      lines(IR.02~year, col="grey", lty=2) 
      lines(IR.98~year, col="grey", lty=2)
      points((MR.mainstem+MR.yentna)~year, type='p', col="red", pch=18) 
      axis(side=1)
      legend(x="topright",c("Mark Recapture"),pch=c(18,19,20),
             col=c("red"),bty="n",text.col=c("red"))
#dev.off()
#par(mfrow=c(1,1))

################################################################################
#  
#  HORSETAIL plots of run size N
#  
################################################################################
#source('H:/My Documents/Bayes/JAGS/source/get_post_function_for_ADFG.r')    

N.smp <- get.post(post, "N[", do.post = T)$posterior; str(N.smp)

keep <- seq(1,4000,by=400); keep
thinned <- N.smp[keep,] 
nrow(thinned) # number of time series to plot
thinned[2,]  # single plausible time series
#png(paste(stock,version,"Plausible_N.png"),width=6.5,height=6.0,units="in",res=1200)

par(mfcol=c(2,1),mar=c(1,4,4,1)+0.1)

color <- rainbow(nrow(thinned)-1); color
plot(cyear,thinned[1,],type="l",ylim=c(0,max(thinned)),ylab='N[t]')
#     ,main='Some Plausible Abundance Time Series')
for(i in 1:nrow(thinned)-1) {
  points(cyear,thinned[i+1,],type="l",col=color[i])
}

plot(TR.mn~cyear, type='b', col="black", pch=15, ps=2, xlim=c(1980,2020), ylim=c(0,max(TR.98)), 
           xaxt='n', ylab="Total Run")
 lines(TR.02~cyear, col="grey", lty=2) 
 lines(TR.98~cyear, col="grey", lty=2)
 axis(side=1)                                    #requires par xaxt='n'


#dev.off()
#par(mfrow=c(1,1))



################################################################################
#  
#  R v S plot w time specific Ricker, year labels
#  
################################################################################
lna.t.mn=stats[substr(rownames(stats),1,12)=="lnalpha.vec[",1]
lna.t.50=quants[substr(rownames(quants),1,12)=="lnalpha.vec[",3]
lna.t.mn/lna.t.50
beta.mn=stats[substr(rownames(stats),1,4)=="beta",1]
beta.50=quants[substr(rownames(quants),1,4)=="beta",3]
#Agamma.mn=stats[substr(rownames(stats),1,7)=="A.gamma",1]                    # v5.11
R     <- R.mn[-c(1:amax)]                   #lag 
S     <- S.mn[1:length(R)]
Rmd     <- R.md[-c(1:amax)]                   #lag 
Smd     <- S.md[1:length(R)]

s0 <- seq(0,300000,by=3000)         #increments of S for plotting

#plot(Smd,Rmd,xlab='Spawners', pch=19, ylab='Recruits', xlim=c(0,max(R.mn)),ylim=c(0,max(R.mn)))
plot(Smd,Rmd,xlab='Spawners', col="white", ylab='Recruits', xlim=c(0,max(R.mn)),ylim=c(0,max(R.mn)))
for (j in 1:length(R)){
#    Rs <- s * exp(lna.t.50[j] - beta.50 * s)
#    Rs <- s * exp(lna.t.mn[j] - beta.mn * s)
#    Rs <- s0 * exp(lna.t.mn[j] - beta.mn * s0 + Agamma.mn * F2dot[j])           # v5.11
    Rs <- s0 * exp(lna.t.mn[j] - beta.mn * s0)           
    lines(s0,Rs,lty=3,lwd = 0.1,col='gray')
    }

abline(0,1)
byear
fbl <- fyr      - 1900
lbl <- lyr-amin - 2000
year.lbl <- c(fbl:99,0:lbl)
#text(S,R, labels=year.lbl[1:length(R)], cex= 0.7, pos=1)    # year labels         
text(Smd,Rmd, labels=year.lbl[1:length(R)], cex= 0.8)    # year labels         

