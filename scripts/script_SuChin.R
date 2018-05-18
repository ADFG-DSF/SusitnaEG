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

packs <- c("SusitnaEG", "rjags", "coda", "ggplot2")
lapply(packs, require, character.only = TRUE)

##Motivating example##
#Sampling programs are bias
#Creel biased toawrds larger fish
age[, 3:7] %>% 
  (function(x) {x/rowSums(x)}) %>% 
  cbind(age[, c("year", "location")]) %>% 
  tidyr::gather(age, p, -year, -location) %>%
  dplyr::mutate(loc2 = ifelse(grepl("creel|Creel", location), "Creel",
                              ifelse(grepl("weir|Weir", location), "Weir", "Other"))) %>%
  dplyr::filter(year >= "1979") %>%
  ggplot(aes(x = year, y = p, color = loc2)) +
  geom_point() +
  facet_grid(age ~ .)

rm(list=ls(all=TRUE))

get_ids()

weir.deshka <- weir[grepl("Deshka", weir$trib), "count"] %>% unlist()

Ha.hat <-
  Ha[, -which(colnames(Ha) %in% c("A", "year"))] %>%
  apply(1, sum, na.rm = TRUE) %>%
  c(., rep(NA, 2))

a <- 
  age[age$year >= 1979, ] %>%
  dplyr::mutate(x678 = x6 + x78,
                samp = ifelse(grepl("creel|Creel", location), 2, ifelse(grepl("weir|Weir", location), 1, 3))) %>% 
  dplyr::left_join(data.frame(yr.a = as.numeric(names(year_id)), year = year_id, stringsAsFactors = FALSE),
                   by = "year") %>%
  dplyr::select(yr.a, samp, x3, x4, x5, x678) %>%
  dplyr::filter(!is.na(x4))
x.a <- as.matrix(a[, grepl("x", names(a))]) 

air.surveys <- as_complete[, !grepl("year|A", colnames(as_complete))] %>% as.matrix()

####  Bundle data to be passed to JAGS  ####
dat = list(
  Y = length(year_id), A = ncol(x.a), a.min = age_min, a.max = age_max, 
  x.a = x.a, n.a = rowSums(x.a), yr.a = a$yr.a, N.yr.a = length(a$yr.a), x.samp = a$samp, #x.creel = a$creel, x.other = a$other,  
  air.surveys = air.surveys, 
  telemetry = telemetry, 
  radios.main = rowSums(telemetry[,1:6]), 
  radios.yentna = rowSums(telemetry[,7:11]),
  Hm.hat = c(Hm$Hm_Susitna, rep(NA, length(year_id) - length(Hm$Hm_Susitna))), cv.hm = rep(0.05, length(year_id)),
  Ha.hat = Ha.hat, cv.ha = rep(0.2, length(Ha.hat)),
  MR.mainstem = mr$mr_mainstem, cv.mrm = mr$cv_mainstem,
  MR.yentna = mr$mr_yentna,     cv.mry = mr$cv_yentna,
  weir.deshka = weir.deshka,
  s3 = rbind(matrix(0, length(year_id) - sum(lt500$age == "1.1"), 2), as.matrix(lt500[lt500$age == "1.1", c("n_small", "n")])),
  s4 = rbind(matrix(0, length(year_id) - sum(lt500$age == "1.2"), 2), as.matrix(lt500[lt500$age == "1.2", c("n_small", "n")]))
)

# bundle inits for JAGS
inits <- list(get_inits(), get_inits())

####  Define the parameters (nodes) of interest  ##### 
parameters=c(
'beta','sigma.white','sigma.R0',
'lnalpha','lnalpha.c','alpha','lnalpha.vec', 
'phi','log.resid.0','log.resid.vec',
'S.eq','S.max','S.msy','U.msy',
'pi.y','D.sum','D.scale','ML1','ML2',
'mu','mu.Hmarine','mu.Habove',
'S','N','R','IR','IR.main','IR.yentna',
'p','N.ta','q', "b", "q.star",
'Bfork.sum','Dtrib.sum','Btheta.sum','Btheta.scale',
'pi.fork.main','pi.fork.yent','pf.main','pf.yentna',
'pi.main','pi.yent','pm','py',
'theta', 'theta.mean', 'sigma.asmain','sigma.asyent','sigma.weir'
)

#### run JAGS ####
ptm = proc.time()
jmod = jags.model(file=".\\models\\mod_SuChin.r", data=dat, n.chains=2, inits=inits, n.adapt=1000)  
#update(jmod, n.iter=1000, by=1, progress.bar='text')               
#post = coda.samples(jmod, parameters, n.iter=5000, thin=1)        # 10 min
#update(jmod, n.iter=2000, by=1, progress.bar='text')               
#post = coda.samples(jmod, parameters, n.iter=20000, thin=10)         
update(jmod, n.iter=100000, by=1, progress.bar='text')               
post = coda.samples(jmod, parameters, n.iter=100000, thin=50)         #  1.5h
#post = coda.samples(jmod, parameters, n.iter=600000, thin=300)       #  
endtime = proc.time()-ptm
endtime[3]/60/60  


#saveRDS(post, file = ".\\posts\\SuChinook_fourages_3ec064bc.rds")
#post <- readRDS(paste0(".\\posts\\", stock, version, ".rds"))

#inspect convergence
shinystan::launch_shinystan(shinystan::as.shinystan(post))

summary <- get_summary(post)

#deshka observed theta and estimated theta track
plot(1979:2017, summary[grepl("theta\\[\\d+,2\\]", rownames(summary)), "Mean"]$Mean, type = "l")
points(1979:2017, air.surveys[,"C"]/weir.deshka)

#Similar mean for each stock to old version
tibble::rownames_to_column(summary) %>% 
  dplyr::filter(grepl("^theta\\[", rowname)) %>%
  dplyr::mutate(year = as.numeric(gsub("theta\\[(\\d+),\\d+\\]", "\\1", rowname)),
                stock = factor(as.numeric(gsub("theta\\[\\d+,(\\d+)\\]", "\\1", rowname)), labels = codes$name[-1])) %>%
  ggplot(aes(x = year, y = Mean)) +
    geom_line() +
    facet_grid(. ~ stock)

plot_theta(get_summary(readRDS(".\\posts\\SuChinook_allagedat96430d7c.rds")))

##changes in q##
new <- get_array(summary, "q") %>%
  tidyr::gather(age, prop, dplyr::starts_with("age")) %>%
  dplyr::mutate(plot = "Age Composition",
                year = as.numeric(year_id[cyear]),
                model = "all data w/beta")
old <- get_array(get_summary(readRDS(".\\posts\\SuChinook_allagedat96430d7c.rds")), "q") %>%
  tidyr::gather(age, prop, dplyr::starts_with("age")) %>%
  dplyr::mutate(plot = "Age Composition",
                year = as.numeric(year_id[cyear]),
                model = "all data")

#estimates unchanged late in time series (only weir data)
#early estimates generaly smaller percentages of age4 and larger percentages of age1 & age2
rbind(new, old) %>%
  ggplot(aes(x = year, y = prop, color = model)) +
  geom_line() +
  facet_grid(age ~ .)

#age at maturity trend maintained
tibble::rownames_to_column(summary) %>% dplyr::filter(grepl("ML", rowname))

table_params(summary)

plot_fit(summary)
plot_state(summary, S_msr = TRUE)
plot_statepairs(post)

#2d age at maturity and age comp arrays
table_age(summary, "p") #age-at-maturity
table_age(summary, "q") #age comp
table_age(summary, "N.ta") #total run by age

plot_age(as.data.frame(x.a), summary)

table_stock(summary)
plot_stock(telemetry, summary)

plot_theta(summary)

plot_horse(post, summary, 325000)
plot_rickeryear(summary)
plot_profile(get_profile(post, 200000))
plot_ey(get_profile(post, 250000))
