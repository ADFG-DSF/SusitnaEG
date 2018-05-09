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

get_ids()

weir.deshka <- weir[grepl("Deshka", weir$trib), "count"] %>% unlist()

Ha.hat <-
  Ha[, -which(colnames(Ha) %in% c("A", "year"))] %>%
  apply(1, sum, na.rm = TRUE) %>%
  c(., rep(NA, 2))

x.a <- 
  age[grepl("Deshka", age$location), ] %>%
  dplyr::mutate(x34 = x3 + x4,
                x678 = x6 + x78) %>%
  dplyr::select(x34, x5, x678) %>%
  as.matrix()
n.a <- rowSums(x.a) %>% ifelse(is.na(.), 100, .) 

air.surveys <- as_complete[, !grepl("year|A", colnames(as_complete))] %>% as.matrix()

####  Bundle data to be passed to JAGS  ####
dat = list(
  Y = length(year_id), A = ncol(x.a), a.min = age_min, a.max = age_max, 
  x.a = x.a, n.a = n.a, 
  air.surveys = air.surveys, 
  telemetry = telemetry, 
  radios.main = rowSums(telemetry[,1:6]), 
  radios.yentna = rowSums(telemetry[,7:11]),
  Hm.hat = c(Hm$Hm_Susitna, rep(NA, length(year_id) - length(Hm$Hm_Susitna))), cv.hm = rep(0.05, length(year_id)),
  Ha.hat = Ha.hat, cv.ha = rep(0.2, length(Ha.hat)),
  MR.mainstem = mr$mr_mainstem, cv.mrm = mr$cv_mainstem,
  MR.yentna = mr$mr_yentna,     cv.mry = mr$cv_yentna,
  weir.deshka = weir.deshka,
  s34 = c(rep(NA, length(year_id) - length(lt500$n_small)), lt500$n_small),
  n34 = c(rep(0, length(year_id) - length(lt500$n)), lt500$n)
)

# bundle inits for JAGS
inits <- list(get_inits(), get_inits())

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

plot_fit(summary)
plot_state(summary)
plot_statepairs(post)

#2d age at maturity and age comp arrays
table_age(summary, "p") #age-at-maturity
table_age(summary, "q") #age comp
table_age(summary, "N.ta") #total run by age

plot_age(as.data.frame(x.a), summary)

table_stock(summary)
plot_stock(telemetry, summary)

plot_horse(post, summary, 325000)
plot_profile(get_profile(post, 200000))
plot_ey(get_profile(post, 250000))
