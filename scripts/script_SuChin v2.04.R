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

packs <- c("dplyr", "tidyr", "ggplot2", "SusitnaEG", "rjags", "coda")
lapply(packs, require, character.only = TRUE)


#options(digits=8)        ## set output digits
#options(scipen=5)        ## reduce scientific notation   

####   Start here to create new version  #######################################################

rm(list=ls(all=TRUE))

stock<-"SuChin"
version <- "v2.04"

alldat=read.csv('.\\models\\SuChin v1.04 data 17Feb18.csv',header=T)

year=as.numeric(as.character(alldat$year))
Hm.hat=as.numeric(alldat$Hm.hat)
cv.hm=as.numeric(alldat$cv.hm)                   
MR.yentna=as.numeric(alldat$MR.yentna)
cv.mrm=as.numeric(alldat$cv.mrm)                   
MR.mainstem=as.numeric(alldat$MR.mainstem)
cv.mry=as.numeric(alldat$cv.mry)                   
weir.deshka = as.numeric(alldat$weir.deshka)
cv.wd=as.numeric(alldat$cv.wd) 

#Harvest by group
Ha.hat <-
  Ha_expand[, -which(colnames(Ha_expand) %in% c("A", "year"))] %>%
  #dplyr::mutate_all(function(x) ifelse(x == 0, 10, x)) %>%
  as.matrix() %>%
  rbind(matrix(NA, nrow = 2, ncol = ncol(.)))

cv.ha=matrix(0.2, nrow = dim(Ha.hat)[1] + 2, ncol = dim(Ha.hat)[2])
 
#age comp count data 
prop.a=as.matrix(alldat[,substr(colnames(alldat), 1,2)=="p."])
colnames(prop.a)=NULL
x.a <- round(prop.a * 100)
n.a=rowSums(x.a)                                           

air.survey <- as_complete[, !grepl("year|A", colnames(as_complete))] %>% as.matrix()

telemetry=as.matrix(telemetry)
radios.main  =rowSums(telemetry[,1:6])                                       
radios.yentna=rowSums(telemetry[,7:11]) 

nyrs=as.numeric(length(year))
fyr=min(year)
lyr=max(year)
nages=3
amin=4
amax=6


                  






####  Bundle data to be passed to JAGS  ####
dat=list(Y = nyrs, A=nages, a.min=amin, a.max=amax,
 x.a=x.a, n.a=n.a, air.surveys=air.surveys, 
 telemetry=telemetry, radios.main=radios.main,radios.yentna=radios.yentna,
 Hm.hat=Hm.hat, cv.hm=cv.hm,
# Hx.hat=Hx.hat, cv.hx=cv.hx,
 Ha.hat=Ha.hat, cv.ha=cv.ha,
 MR.mainstem=MR.mainstem, cv.mrm=cv.mrm,
 MR.yentna=MR.yentna,     cv.mry=cv.mry,
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
jmod = jags.model(file=model_file_loc, data=dat, n.chains=2, inits=inits, n.adapt=1000)  
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
save(post,file=paste(stock,version,"post") ) 
summary=summary(post); str(summary)

#Adam Reimer's GET.POST function   
get.post.adam=function(post_dat, var){
  #coerce to matrix if mcmc.list
  if (!coda::is.mcmc.list(post_dat) & !is.matrix(post_dat)) stop("post_dat is not of class mcmc.list or matrix")
  if (coda::is.mcmc.list(post_dat)) post_dat=as.matrix(post_dat)
  #pull out posteriors for requested variable
  if(substr(var,nchar(var), nchar(var))=="[") post=post_dat[,substr(colnames(post_dat), 1, nchar(var))==var] else post=post_dat[,var]
  post
}
#### Use Ben Staton's GET.POST function to inspect select nodes ########## 
source('H:/My Documents/Bayes/JAGS/source/get_post_function_for_ADFG.r')    
      get.post(post, "ML1[", do.plot=T)
      get.post(post, "ML2[", do.plot=T)
      get.post(post, "N[39]", do.plot=T)
      get.post(post, "R[", do.plot=T)
      get.post(post, "Dtrib.sum", do.plot=T)  
      get.post(post, "D.sum", do.plot=T)  
      get.post(post, "Bfork.sum", do.plot=T)  
      get.post(post, "pi.fork.main", do.plot=T)  
      get.post(post, "pf.main[", do.plot=T)  
      get.post(post, "Btheta.sum", do.plot=T)  
      get.post(post, "theta[", do.plot=T)  
      get.post(post, "pi.main[", do.plot=T)  
      get.post(post, "pf.main[", do.plot=T)  
      get.post(post, "pm[", do.plot=T)  
      get.post(post, "pi.yent[", do.plot=T)  
      get.post(post, "pf.main[", do.plot=T)  
      get.post(post, "beta", do.plot=T)  
      get.post(post, "lnalpha", do.plot=T)  
      get.post(post, "phi", do.plot=T)  
      get.post(post, "sigma.trib", do.plot=T)  
      get.post(post, "sigma.white", do.plot=T)  
      get.post(post, "sigma.R0", do.plot=T)  
      get.post(post, "S.max", do.plot=T)
      get.post(post, "S.eq", do.plot=F)
      get.post(post, "S.msy", do.plot=F)
      get.post(post, "log.resid.vec[", do.plot=T)
      get.post(post, "log.resid.0", do.plot=T)
      get.post(post, "mu.Habove[", do.plot=T)
      get.post(post, "mu.Hmarine[", do.plot=T)
      get.post(post, "N[", do.plot=T)
      get.post(post, "S[", do.plot=T)
      get.post(post, "R[", do.plot=T)


#### Find nodes with POOR convergence ##########################
gel=as.data.frame(gelman.diag(post, multivariate=F)[[1]])
gel
poor.threshold=1.10#values less than 1.2 are generally considered converged
poor=matrix(NA, nrow=nrow(gel), ncol=2)
for(i in 1:nrow(gel)){
  if(gel[i,1]>poor.threshold){
    poor[i,1]=rownames(gel[i,])
    poor[i,2]=round(gel[i,1],2)}
}
poor=poor[!is.na(poor[,1]),]
poor=as.data.frame(poor); colnames(poor)=c("Parameter", "Gelman-Rubin")
poor

#### PLOT traces and density of all monitored nodes to pdf) ####
#windows(record=T) 
#pdf(paste(stock,version,"trace.pdf"),onefile=T,useDingbats=F)
#plot(post)
#dev.off()

#### CALCULATE DIC criteria ########################################
dic.pD  <-dic.samples(jmod,n.iter=10000,thin=10,"pD")
dic.popt<-dic.samples(jmod,n.iter=10000,thin=10,"popt")
dev1 <- sum(dic.pD[[1]])
pD   <- sum(dic.pD[[2]])
dic.pD <- dev1 + pD
dic.pD.summary <- c(dev1, pD, dic.pD)
write(dic.pD.summary, file=paste("YukCanChin",version,"_DIC_pD.txt") )  
      

#### WRITE posterior samples and summaries to disk ########################
#save(post,file=paste(stock,version,"post") ) 
#load(file=paste(stock,version,"post"))

#summary=summary(post); str(summary)
stats=summary$statistics;      colnames(stats)
quants=summary$quantiles;      colnames(quants)
statsquants = cbind(stats,quants)       # combine for output file
write.csv(statsquants, file= paste(stock,version," statsquants.csv") )    # writes csv file

#### COLLECT posterior stats for 2d age at maturity and age comp arrays ###########
cyear     <- year                           # calendar year vector
byear     <- (fyr-amax):(lyr-amin)          # brood years vector
ryear     <- fyr:(lyr-amin)                 # resid years vector
cyrs      <- length(cyear) 
byrs      <- length(byear)
ryrs      <- length(ryear)

pnames <- rownames(stats[substr(rownames(stats),1,2)=="p[",])
p.mn=stats[substr(rownames(stats),1,2)=="p[",1]
P01=data.frame(P=p.mn,row.names=pnames)                  # p dataframe with names
P02=data.frame(expand.grid(BYEAR=1:byrs,AGE=1:3),P=p.mn) # add byear and age columns
P03=tapply(P02$P,P02$BYEAR,t)                            # transpose by year to create list of 3-element vectors
P.mn=data.frame(BYEAR=1:length(P03),AGE1=-999,AGE2=-999,AGE3=-999) # Dummy data.frame-fill in below
i=1
while(i<=length(P03)){P.mn[i,2:4]=P03[[i]]; i=i+1 }

pinames <- rownames(stats[substr(rownames(stats),1,5)=="pi.y[",])
pi.mn=stats[substr(rownames(stats),1,5)=="pi.y[",1]
Pi01=data.frame(Pi=pi.mn,row.names=pinames)                  # p dataframe with names
Pi02=data.frame(expand.grid(BYEAR=1:byrs,AGE=1:3),Pi=pi.mn)  # add byear and age columns
Pi03=tapply(Pi02$Pi,Pi02$BYEAR,t)                            # transpose by year to create list of 3-element vectors
Pi.mn=data.frame(BYEAR=1:length(Pi03),AGE1=-999,AGE2=-999,AGE3=-999) # Dummy data.frame-fill in below
i=1
while(i<=length(P03)){Pi.mn[i,2:4]=Pi03[[i]]; i=i+1 }

qnames <- rownames(stats[substr(rownames(stats),1,2)=="q[",])
q.mn=stats[substr(rownames(stats),1,2)=="q[",1]
Q01=data.frame(Q=q.mn,row.names=qnames)                  # q dataframe with names
Q02=data.frame(expand.grid(CYEAR=1:cyrs,AGE=1:3),Q=q.mn) # add byear and age columns
Q03=tapply(Q02$Q,Q02$CYEAR,t)                            # transpose by year to create list of 3-element vectors
Q.mn=data.frame(CYEAR=1:length(Q03),AGE1=-999,AGE2=-999,AGE3=-999) # Dummy data.frame-fill in below
i=1
while(i<=length(Q03)){Q.mn[i,2:4]=Q03[[i]]; i=i+1 }

Nnames <- rownames(stats[substr(rownames(stats),1,5)=="N.ta[",])
n.mn=stats[substr(rownames(stats),1,5)=="N.ta[",1]
N01=data.frame(N=n.mn,row.names=Nnames)                  # q dataframe with names
N02=data.frame(expand.grid(CYEAR=1:cyrs,AGE=1:3),N=n.mn)   # add cyear and age columns
N03=tapply(N02$N,N02$CYEAR,t)                            # transpose by year to create list of 3-element vectors
N.mn=data.frame(CYEAR=1:length(N03),AGE1=-999,AGE2=-999,AGE3=-999) # Dummy data.frame-fill in below
i=1
while(i<=length(N03)){N.mn[i,2:4]=N03[[i]]; i=i+1 }

#### COLLECT posterior stats for mainstem and yentna stock comp arrays ###########

pmnames <- rownames(stats[substr(rownames(stats),1,3)=="pm[",])
pm.mn=stats[substr(rownames(stats),1,3)=="pm[",1]
Pm01=data.frame(Pm=pm.mn,row.names=pmnames)                  # p dataframe with names
Pm02=data.frame(expand.grid(CYEAR=1:cyrs,TRIB=1:6),Pm=pm.mn) # add byear and age columns
Pm03=tapply(Pm02$Pm,Pm02$CYEAR,t)                            # transpose by year to create list of 6-element vectors
Pm.mn=data.frame(CYEAR=1:length(Pm03),TRIB1=-999,TRIB2=-999,TRIB3=-999,TRIB4=-999,TRIB5=-999,TRIB6=-999) 
i=1
while(i<=length(Pm03)){Pm.mn[i,2:7]=Pm03[[i]]; i=i+1 }

pynames <- rownames(stats[substr(rownames(stats),1,3)=="py[",])
py.mn=stats[substr(rownames(stats),1,3)=="py[",1]
Py01=data.frame(Py=py.mn,row.names=pynames)                  # p dataframe with names
Py02=data.frame(expand.grid(CYEAR=1:cyrs,TRIB=1:5),Py=py.mn) # add byear and age columns
Py03=tapply(Py02$Py,Py02$CYEAR,t)                            # transpose by year to create list of 5-element vectors
Py.mn=data.frame(CYEAR=1:length(Py03),TRIB1=-999,TRIB2=-999,TRIB3=-999,TRIB4=-999,TRIB5=-999) 
i=1
while(i<=length(Py03)){Py.mn[i,2:6]=Py03[[i]]; i=i+1 }


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
#   START WRITE TO PDF
#  
################################################################################

pdf(paste(stock,version,"output.pdf"),width=6.5,height=9.5)

################################################################################
#  
#  SCATTER PLOT MATRIX of abundance index data
#  
################################################################################
#dev.off()      
#png(paste(stock,version,"ScatterPlotData.png"),width=9.5,height=7.0,units="in",res=1200)
par(mfrow=c(1,1))
indices <- cbind(as.deshka,as.east.su,as.talkeetna, as.main.up,as.chulitna,as.lake,as.kahiltna,as.talachulitna,weir.deshka,MR.mainstem,MR.yentna)
pairs(indices,cex=0.6)
#dev.off()
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
#  State Variables Panel
#  
################################################################################
#par(mfrow=c(3,1),mar=c(1,4,4,1)+0.1,ps=12)    #SAVE
#dev.off()

#png(paste(stock,version,"StateVarsPanel.png"),width=6.5,height=8.5,units="in",res=1200)
par(mfrow=c(5,1),mar=c(1,4,1,1)+0.1, ps=12, lwd=1)

      plot(S.mn~cyear, type='b', col="black", pch=15, ps=2, xlim=c(1980,2020), ylim=c(0, max(S.98)),
           ylab="Escapement" )#, xaxt='n')
      lines(S.02~cyear, col="grey", lty=2) 
      lines(S.98~cyear, col="grey", lty=2)
      abline(h=Seq.50,col="red",lty=1)
      abline(h=Smax.50,col="red",lty=2)
      abline(h=Smsy.50,col="red",lty=3)
      legend(x="topleft",c("Seq","Smax","Smsy"),lty=1:3,col="red",bty="n",text.col="red")

      plot(R.mn~byear, type='b', col="black", pch=15, ps=2, xlim=c(1980,2020), ylim=c(0,max(R.98)), 
           xaxt='n', ylab="Recruitment")
      lines(R.02~byear, col="grey", lty=2) 
      lines(R.98~byear, col="grey", lty=2)

      plot(TR.mn~cyear, type='b', col="black", pch=15, ps=2, xlim=c(1980,2020), ylim=c(0,max(TR.98)), 
           xaxt='n', ylab="Total Run")
      lines(TR.02~cyear, col="grey", lty=2) 
      lines(TR.98~cyear, col="grey", lty=2)
      axis(side=1)                                    #requires par xaxt='n'

      plot(resid.mn~ryear, type='b', col="black", pch=15, ps=2, xlim=c(1980,2020),
           ylim=c(min(resid.02),max(resid.98)), xaxt='n', ylab="Recruitment Resids")
      lines(resid.02~ryear, col="grey", lty=2) 
      lines(resid.98~ryear, col="grey", lty=2)
      abline(h=0,col="red",lty=2)
      legend(x="topleft",c("Average Productivity"),lty=3,col="red",bty="n",text.col="red")

      par(mar=c(2,4,1,1)+0.1)
      plot(mu.mn~cyear, type='b', col="black", pch=15, ps=2, xlim=c(1980,2020), ylim=c(0,1), 
           xaxt='n', ylab="Harvest Rate", xlab="Year")
      lines(mu.02~cyear, col="grey", lty=2) 
      lines(mu.98~cyear, col="grey", lty=2)
      abline(h=Umax.50,col="red",lty=2)
      abline(h=Umsy.50,col="red",lty=3)
      axis(side=1, at=cyear, labels=year)
      legend(x="topleft",c("Umax","Umsy"),lty=c(2,3),col="red",bty="n",text.col="red")

#par(mfrow=c(1,1)); dev.off()
#

################################################################################
#  
#  Age Comp and Abundance by Age Plots
#  
################################################################################


#png(paste(stock,version,"AgePanel.png"),width=6.5,height=8.5,units="in",res=1200)
par(mfrow=c(3,1),mar=c(1,4,4,1)+0.1,ps=12)

pi4.fit <- Pi.mn$AGE1
pi45.fit <- Pi.mn$AGE1 + Pi.mn$AGE2
pi456.fit <- Pi.mn$AGE1 + Pi.mn$AGE2 + Pi.mn$AGE3

p4.fit <- P.mn$AGE1
p45.fit <- P.mn$AGE1 + P.mn$AGE2
p456.fit <- P.mn$AGE1 + P.mn$AGE2 + P.mn$AGE3

Q.obs <- x.a/n.a
q4.obs <- Q.obs[,1]
q45.obs <- Q.obs[,1] + Q.obs[,2]
q456.obs <- Q.obs[,1] + Q.obs[,2] + Q.obs[,3]

q4.fit <- Q.mn$AGE1
q45.fit <- Q.mn$AGE1 + Q.mn$AGE2
q456.fit <- Q.mn$AGE1 + Q.mn$AGE2 + Q.mn$AGE3

plot(p456.fit~byear, type='l', col="black", pch=1, ps=2, xlim=c(1980,2020), ylim=c(0, 1), ylab="Proportion",
     main="Age at Maturity", lty=2)
  lines(p45.fit~byear, lty=2) 
  lines(p4.fit~byear, lty=2) 
  lines(pi456.fit~byear, lty=1) 
  lines( pi45.fit~byear, lty=1) 
  lines(  pi4.fit~byear, lty=1) 

plot(q456.obs~cyear, type='p', col="black", pch=1, ps=2, xlim=c(1980,2020), ylim=c(0, 1), ylab="Proportion",
     main="Age Composition")
  points(q45.obs~cyear)
  points(q4.obs~cyear)
  lines(q456.fit~cyear, lty=2) 
  lines(q45.fit~cyear, lty=2) 
  lines(q4.fit~cyear, lty=2) 

N4.fit <- N.mn$AGE1
N45.fit <- N.mn$AGE1 + N.mn$AGE2
N456.fit <- N.mn$AGE1 + N.mn$AGE2 + N.mn$AGE3
plot(N456.fit~cyear, main="Total Run by Age", type='l', lty=3, col="black", pch=15, ylim=c(0,max(TR.mn)), 
     xlim=c(1980,2020), xaxt='n', xlab="Year", ylab="Number of Chinook")
  lines(N45.fit~cyear, type='l', lty=2, col="black",  ps=1)
  lines(N4.fit~cyear, type='l', lty=2, col="black",  ps=1)
  points(TR.mn~cyear, type='l', lty=2, col="black",  ps=1)
  axis(side=1, at=cyear, labels=cyear)

#par(mfrow=c(1,1))
#dev.off()

################################################################################
#  
#  Stock Composition Plots
#  
################################################################################

#png(paste(stock,version,"StockPanel.png"),width=6.5,height=8.5,units="in",res=1200)
par(mfrow=c(3,1),mar=c(1,4,4,1)+0.1,ps=12)

pf.obs <- MR.mainstem/(MR.mainstem + MR.yentna)
pfmain.50=quants[substr(rownames(quants),1,8)  =="pf.main[",3]
pfmain.02=quants[substr(rownames(quants),1,8)  =="pf.main[",1]
pfmain.98=quants[substr(rownames(quants),1,8)  =="pf.main[",5]

plot(pfmain.50~cyear, type='l', col="black", pch=1, ps=2, xlim=c(1978,2018), ylim=c(0, 1), ylab="Proportion",
     main="Mainstem Proportion")
points(pf.obs~cyear, col="red")
lines(pfmain.02~cyear, lty=3) 
lines(pfmain.98~cyear, lty=3) 


pm1.fit <- Pm.mn$TRIB1
pm12.fit <- Pm.mn$TRIB1 + Pm.mn$TRIB2
pm123.fit <- Pm.mn$TRIB1 + Pm.mn$TRIB2 + Pm.mn$TRIB3 
pm1234.fit <- Pm.mn$TRIB1 + Pm.mn$TRIB2 + Pm.mn$TRIB3 + Pm.mn$TRIB4 
pm12345.fit <- Pm.mn$TRIB1 + Pm.mn$TRIB2 + Pm.mn$TRIB3 + Pm.mn$TRIB4 + Pm.mn$TRIB5

Pm.obs <- telemetry[,1:6]/radios.main
pm1.obs <- Pm.obs[,1]
pm12.obs <- Pm.obs[,1] + Pm.obs[,2]
pm123.obs <- Pm.obs[,1] + Pm.obs[,2] + Pm.obs[,3] 
pm1234.obs <- Pm.obs[,1] + Pm.obs[,2] + Pm.obs[,3] + Pm.obs[,4] 
pm12345.obs <- Pm.obs[,1] + Pm.obs[,2] + Pm.obs[,3] + Pm.obs[,4] + Pm.obs[,5] 

plot(pm12345.obs~cyear, type='p', col="black", pch=1, ps=2, xlim=c(1978,2018), ylim=c(0, 1), ylab="Proportion",
     main="Mainstem Stock Composition")
  points(pm1234.obs~cyear, col="red")
  points(pm123.obs~cyear, col="blue")
  points(pm12.obs~cyear, col="green")
  points(pm1.obs~cyear)
  lines(pm12345.fit~cyear, lty=2) 
  lines(pm1234.fit~cyear, lty=2, col="red") 
  lines(pm123.fit~cyear, lty=2, col="blue") 
  lines(pm12.fit~cyear, lty=2, col="green") 
  lines(pm1.fit~cyear, lty=2) 

py1.fit <- Py.mn$TRIB1
py12.fit <- Py.mn$TRIB1 + Py.mn$TRIB2
py123.fit <- Py.mn$TRIB1 + Py.mn$TRIB2 + Py.mn$TRIB3 
py1234.fit <- Py.mn$TRIB1 + Py.mn$TRIB2 + Py.mn$TRIB3 + Py.mn$TRIB4 

Py.obs <- telemetry[,7:11]/radios.yentna
py1.obs <- Py.obs[,1]
py12.obs <- Py.obs[,1] + Py.obs[,2]
py123.obs <- Py.obs[,1] + Py.obs[,2] + Py.obs[,3] 
py1234.obs <- Py.obs[,1] + Py.obs[,2] + Py.obs[,3] + Py.obs[,4] 

plot(py1234.obs~cyear, type='p', col="black", pch=1, ps=2, xlim=c(1978,2018), ylim=c(0, 1), ylab="Proportion",
     main="Yentna Stock Composition")
  points(py123.obs~cyear, col="red")
  points(py12.obs~cyear, col="blue")
  points(py1.obs~cyear)
  lines(py1234.fit~cyear, lty=2) 
  lines(py123.fit~cyear, lty=2, col="red") 
  lines(py12.fit~cyear, lty=2, col="blue") 
  lines(py1.fit~cyear, lty=2) 

#par(mfrow=c(1,1))
#dev.off()
  
  
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
#  SCATTER PLOT MATRIX of posterior correlations
#  
################################################################################
#dev.off()      
#png(paste(stock,version,"ScatterPlotMatrix.png"),width=9.5,height=7.0,units="in",res=1200)

postdf <- as.data.frame(as.matrix(post))   
#namesdf <- names(postdf)
numkeep <- 200
#nodes2=c('beta','lnalpha','phi','S.msy','A.gamma')
nodes2=c('beta','lnalpha','phi','S.msy','sigma.white')
subset2 = postdf[,nodes2]
subset3 = subset2[subset2$S.msy>Smsy.02,]
subset4 = subset3[subset3$S.msy<Smsy.98,]
str(subset4)
rows1 <- length(subset4[,1]); #rows1
int <- rows1 / numkeep; #int
keep <- seq(1,rows1,by=int); #keep
thinned <- subset4[keep,]; str(thinned)
sampnum=1:length(thinned[,1]); #sampnum

par(mfrow=c(1,1))
pairs(thinned,cex=0.6)
#dev.off()

################################################################################
#  
#  R v S plot w "horsetails", error bars, year labels
#  
################################################################################
S.smp <- get.post(post, "S[", do.post = T)$posterior
R.smp <- get.post(post, "R[", do.post = T)$posterior

lna.smp <- get.post(post, "lnalpha", do.post = T)$posterior
beta.smp <- get.post(post, "beta", do.post = T)$posterior
lnab.smp <- cbind(lna.smp,beta.smp)
Smsy.smp <- get.post(post, "S.msy", do.post = T)$posterior
lnac.smp <- get.post(post, "lnalpha.c", do.post = T)$posterior

samples <- length(lna.smp); samples

R     <- R.mn[-c(1:amax)]                   #omit first a.max values
S     <- S.mn[1:length(R)]
S.05 <- numeric(length(R))
S.50 <- numeric(length(R))
S.95 <- numeric(length(R))
R.05 <- numeric(length(R))
R.50 <- numeric(length(R))
R.95 <- numeric(length(R))
str(S.smp)
str(R.smp)
for (t in 1:length(R)){                 # 90% envelope for S 
    S.05[t] <- quantile(S.smp[,t],0.05)
    S.95[t] <- quantile(S.smp[,t],0.95)
    S.50[t] <- quantile(S.smp[,t],0.5)
    R.05[t] <- quantile(R.smp[,t+amax],0.05)
    R.95[t] <- quantile(R.smp[,t+amax],0.95)
    R.50[t] <- quantile(R.smp[,t+amax],0.5)
    }


#s <- seq(0,1.25*Seq.50[1],by=100)         #increments of S for plotting
s <- seq(0,300000,by=5000)         #increments of S for plotting

Rs.md <- numeric(length(s))
Rs.02 <- numeric(length(s))
Rs.98 <- numeric(length(s))

Rs<-matrix(NA, nrow=length(s), ncol=samples)
SY<-matrix(NA, nrow=length(s), ncol=samples)

for (j in 1:length(s)){                 # OYP calculations
    for (k in 1:samples){               
        Rs[j,k] <- s[j]  * exp(lnac.smp[k]  - beta.smp[k] * s[j])
        SY[j,k] <- Rs[j,k] - s[j]
        }
    }

for (j in 1:length(s)){                 # 95% envelope for E[R] by increments in S
    Rs.md[j] <- quantile(s[j] * exp(lna.smp - beta.smp * s[j]),0.5)
    Rs.02[j] <- quantile(s[j] * exp(lna.smp - beta.smp * s[j]),0.025)
    Rs.98[j] <- quantile(s[j] * exp(lna.smp - beta.smp * s[j]),0.975)
    }


n.keep <- 40
int <- samples / n.keep; 
rows.kept <- seq(1,samples,by=int); rows.kept
lnab.thin <- lnab.smp[rows.kept,]; str(lnab.thin)


#par(mfrow=c(1,1),mar = c(8, 3, 15, 2))  
par(mfrow=c(1,1),mar = c(4, 3, 5, 2))  

plot(S.50,R.50,xlab='Spawners', pch=19, ylab='Recruits', xlim=c(0,max(R.mn)),ylim=c(0,max(R.mn)))
for (j in 1:n.keep){
    Rs <- s * exp(lna.smp[j] - beta.smp[j] * s)
    lines(s,Rs,lty=3,lwd = 0.1,col='gray')
    }
points(Rs.md~s)

arrows(R,x0=S.95,x1=S.05,code=0,lty=2,lwd=0.6)  #horiz err bar
arrows(S,y0=R.95,y1=R.05,code=0,lty=2,lwd=0.6)  #vert err bar
abline(0,1)
text(S,R, labels=cyear[1:length(R)], cex= 0.7, pos=1)    # year labels         

################################################################################
#  Yield Panel
#  Optimal Yield Profiles
#  Escapement Frequency Distribution
#  Expected Yield Plot
################################################################################
#pdf(paste(stock,version,"yieldplot.pdf"),width=6.5,height=9.5)
par(mfrow=c(2,1),mar=c(2,4,4,1)+0.1,ps=12)

probMSY70 <- numeric(length(s))
probMSY80 <- numeric(length(s))
probMSY90 <- numeric(length(s))
mean.SY <- numeric(length(s))
median.SY <- numeric(length(s))
p10.SY <- numeric(length(s))
p90.SY <- numeric(length(s))
p25.SY <- numeric(length(s))
p75.SY <- numeric(length(s))
Rmsy.smp <- numeric(samples)
MSY.smp <- numeric(samples)

increments_s <- length(s); increments_s
r  <- matrix(NA, nrow=increments_s,  ncol=samples)
SY<-matrix(NA, nrow=length(s), ncol=samples)
gtMSY70<-matrix(NA, nrow=length(s), ncol=samples)
gtMSY80<-matrix(NA, nrow=length(s), ncol=samples)
gtMSY90<-matrix(NA, nrow=length(s), ncol=samples)

for (k in 1:samples){               
    Rmsy.smp[k] <- Smsy.smp[k] * exp(lnac.smp[k] - beta.smp[k] * Smsy.smp[k])
    MSY.smp[k] <- Rmsy.smp[k] - Smsy.smp[k]
    }
for (j in 1:increments_s){
    for (k in 1:samples){               
        r[j,k] <- s[j] * exp(lnac.smp[k] - beta.smp[k] * s[j])
        SY[j,k] <- r[j,k] - s[j]
        gtMSY70[j,k] <- (SY[j,k] - 0.7 * MSY.smp[k]) > 0
        gtMSY80[j,k] <- (SY[j,k] - 0.8 * MSY.smp[k]) > 0
        gtMSY90[j,k] <- (SY[j,k] - 0.9 * MSY.smp[k]) > 0
        }
    }
for (j in 1:length(s)){                 
    probMSY70[j] <- mean(gtMSY70[j,])
    probMSY80[j] <- mean(gtMSY80[j,])
    probMSY90[j] <- mean(gtMSY90[j,])
    mean.SY[j] <- mean(SY[j,])
    median.SY[j] <- median(SY[j,])
    p10.SY[j] <- quantile(SY[j,],probs=0.10,na.rm=TRUE)
    p90.SY[j] <- quantile(SY[j,],probs=0.90,na.rm=TRUE)
    p25.SY[j] <- quantile(SY[j,],probs=0.25,na.rm=TRUE)
    p75.SY[j] <- quantile(SY[j,],probs=0.75,na.rm=TRUE)
    }

# Optimal Yield Profiles
par(mfrow=c(3,1),mar=c(2,4,4,1)+0.1,ps=12)
#plot(probMSY90~s, type="l",lty=1,xlim=c(0,max(s)),ylim=c(0,1))
plot(probMSY90~s, ylab="Pr(SY > 0.9 MSY)",type="l",lty=1,xlim=c(0,200000),ylim=c(0,1))
lines(probMSY80~s, lty=2)
lines(probMSY70~s, lty=3)
abline(v=c(0.77*Smsy.50,0.77*Smsy.50),col="red",lty=1) # RED LINE IS AVERAGE OF 22 CHINOOK LOWER BOUNDS

# Histogram of Annual Eggs Spawned
hist(S.mn,xlim=c(0,200000),main="",ylab="Freq (Escapement)",nclass=12)
abline(v=c(0.77*Smsy.50,0.77*Smsy.50),col="red",lty=1)  # RED LINE IS AVERAGE OF 22 CHINOOK LOWER BOUNDS

# Expected Yield Plot
plot(median.SY~s, type="l",lty=1,xlim=c(0,200000),ylim=c(0,max(p75.SY)))
lines(p25.SY~s, lty=2)
lines(p75.SY~s, lty=2)
abline(v=c(0.77*Smsy.50,0.77*Smsy.50),col="red",lty=1)
tex <- c(paste('0.77*Smsy= ',round(0.77*Smsy.50,0))) 
legend(x="topright",c("75th %ile","50th %ile","25th %ile"),lty=c(2,1,2),
       col="black",bty="n", text.col="black")

par(mfrow=c(1,1))


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


dev.off() 

################################################################################
#  
#  END WRITE TO PDF
#  Next write stats
#  
################################################################################

yield.stats=cbind(s,probMSY90,probMSY80,probMSY70,p25.SY,median.SY,p75.SY)
write.csv(yield.stats,paste(stock,version,"yield.csv"))

postdf <- as.data.frame(as.matrix(post))   
#namesdf <- names(postdf)
#colnames(postdf)

nodes1<- c('N[39]',
'beta','sigma.white','sigma.R0','lnalpha','lnalpha.c','alpha', 
'phi','S.eq','S.max','S.msy','U.msy','D.sum','D.scale','Dtrib.sum','Btheta.sum','Bfork.sum',
'pi.fork.main','sigma.asmain','sigma.asyent','sigma.weir',
'pi.main[1]','pi.main[2]','pi.main[3]','pi.main[4]','pi.main[5]','pi.main[6]',
'pi.yent[1]','pi.yent[2]','pi.yent[3]','pi.yent[4]','pi.yent[5]',
'theta[1]','theta[2]','theta[3]','theta[4]','theta[5]','theta[6]',
'theta[7]','theta[8]','theta[9]','theta[10]','theta[11]',
'ML1[1]','ML1[2]','ML1[3]','ML2[1]','ML2[2]','ML2[3]')
subset1 = postdf[,nodes1]

p.mn<-apply(subset1,2,mean)
p.sd<-apply(subset1,2,sd)
p.50<-apply(subset1,2,quantile,0.50)
p.25<-apply(subset1,2,quantile,0.25)
p.75<-apply(subset1,2,quantile,0.75)
p.10<-apply(subset1,2,quantile,0.10)
p.90<-apply(subset1,2,quantile,0.90)
p.05<-apply(subset1,2,quantile,0.05)
p.95<-apply(subset1,2,quantile,0.95)
p.02<-apply(subset1,2,quantile,0.025)
p.98<-apply(subset1,2,quantile,0.975)
p.stats.mat<-cbind(p.mn,p.sd,p.02,p.05,p.10,p.25,p.50,p.75,p.90,p.95,p.98)
str(p.stats.mat)
options(scipen=0)
p.stats.tab <- signif(p.stats.mat,digits=3)
write.csv(p.stats.tab,paste(stock,version,"Rounded Stats.csv"))


