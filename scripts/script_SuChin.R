packs <- c("SusitnaEG", "rjags", "coda", "ggplot2")
lapply(packs, require, character.only = TRUE)

rm(list=ls(all=TRUE))

get_ids()

Ha.hat <- Ha %>% rbind(matrix(apply(Ha[(dim(Ha)[1] - 2):dim(Ha)[1], ], MARGIN = 2, mean),
                              byrow = TRUE,
                              nrow = length(year_id) - dim(Ha)[1], 
                              ncol = dim(Ha)[2]))

a <- 
  age[age$year >= 1979, ] %>%
  dplyr::mutate(x678 = x6 + x78,
                samp = ifelse(grepl("creel|Creel", location), 2, ifelse(grepl("weir|Weir", location), 1, 3))) %>% 
  dplyr::left_join(data.frame(yr.a = as.numeric(names(year_id)), year = year_id, stringsAsFactors = FALSE),
                   by = "year") %>%
  dplyr::select(yr.a, samp, x3, x4, x5, x678) %>%
  dplyr::filter(!is.na(x4))
x.a <- as.matrix(a[, grepl("x", names(a))]) 

####  Bundle data to be passed to JAGS  ####
dat = list(
  Y = length(year_id), A = ncol(x.a), a.min = age_min, a.max = age_max, 
  x.a = x.a, n.a = rowSums(x.a), yr.a = a$yr.a, N.yr.a = length(a$yr.a), x.samp = a$samp, 
  tele.S2 = telemetry$'East Susitna', tele.S3 = telemetry$Talkeetna, tele.S4 = telemetry$Yentna, tele.S5 = telemetry$Other,
  Ntele.S2 = telemetry$'N_East Susitna', Ntele.S3 = telemetry$N_Talkeetna, Ntele.S4 = telemetry$N_Yentna, Ntele.S5 = telemetry$N_Other,
  air.S1 = as.vector(as[[1]]), air.S2 = as[[2]], air.S3 = as[[3]], air.S4 = as[[4]], air.S5 = as[[5]], 
  Hm.hat = c(Hm$Hm_Susitna, rep(NA, length(year_id) - length(Hm$Hm_Susitna))), cv.Hm = rep(0.05, length(year_id)),
  Ha.hat = Ha.hat, cv.Ha = rep(0.2, dim(Ha.hat)[1]),
  MR = mr, cv.MR = rep(0.15, dim(mr)[1]),
  weir = weir,
  small3 = rbind(matrix(0, length(year_id) - sum(lt500$age == "1.1"), 2), as.matrix(lt500[lt500$age == "1.1", c("n_small", "n")])),
  small4 = rbind(matrix(0, length(year_id) - sum(lt500$age == "1.2"), 2), as.matrix(lt500[lt500$age == "1.2", c("n_small", "n")]))
)

# bundle inits for JAGS
inits <- list(get_inits(), get_inits())

####  Define the parameters (nodes) of interest  ##### 
parameters=c(
'sigma.white', 'sigma.R0', 'sigma.air', 'B', 'sigma.weir',
'beta', 'lnalpha', 'mu.lnalpha', 'sigma.lnalpha', 'lnalpha.c', 'alpha', 'lnalpha.vec', 
'phi', 'mu.phi', 'sigma.phi', 'log.resid.0', 'log.resid.vec',
'S.eq', 'S.max', 'S.msy', 'U.msy',
'p', 'pi', 'Dsum.age', 'ML1', 'ML2',
'S','N','R','IR',
'N.ta','q', 'b', 'q.star',
'Dsum.S2', 'ML1.S2', 'ML2.S2', 'Dsum.S3', 'ML1.S3', 'ML2.S3', 'Dsum.S4', 'ML1.S4', 'ML2.S4', 'Dsum.S5', 'ML1.S5', 'ML2.S5', 
'p.S2', 'p.S3', 'p.S4', 'p.S5', 'Bsum.So',
'theta', 'b1.theta',
'p.small3', 'p.small4', 
'mu.Hmarine', 'mu.Habove'
)

#### run JAGS ####
#ptm = proc.time()
jmod = jags.model(file=".\\models\\mod_SuChin.r", data=dat, n.chains=2, inits=inits, n.adapt=1000)  
update(jmod, n.iter=1000, by=1, progress.bar='text')               
post = coda.samples(jmod, parameters, n.iter=1000, thin=1)        # 10 min
update(jmod, n.iter=15000, by=1, progress.bar='text')               
post = coda.samples(jmod, parameters, n.iter=10000, thin=10)         
update(jmod, n.iter=100000, by=1, progress.bar='text')               
post = coda.samples(jmod, parameters, n.iter=100000, thin=50)         #  1.5h
#post = coda.samples(jmod, parameters, n.iter=600000, thin=300)       #  
endtime = proc.time()-ptm
endtime[3]/60/60  


saveRDS(post, file = ".\\posts\\SuChinook_3yrHa_07685df.rds")
#post <- readRDS(".\\posts\\SuChinook_3yrHa_07685df.rds")

#inspect convergence
shinystan::launch_shinystan(shinystan::as.shinystan(post))

summary <- get_summary(post)


#age at maturity trend maintained
tibble::rownames_to_column(summary) %>% dplyr::filter(grepl("ML", rowname)) %>% print(n = 100)

table_params(summary)

lapply(stock_id, plot_fit, stats_dat = summary)
lapply(stock_id, function(x) plot_state(summary, stock = x))
plot_statepairs(post)

#2d age at maturity and age comp arrays
tibble::rownames_to_column(summary) %>% dplyr::filter(grepl("b\\[", rowname))
table_age(summary, "p") #age-at-maturity
table_age(summary, "q") #age comp
table_age(summary, "N.ta") #total run by age

plot_age(as.data.frame(x.a), summary)

table_stock(summary)
plot_stock(telemetry, summary)

plot_theta(summary)
table_airerror(summary)
tibble::rownames_to_column(summary) %>% dplyr::filter(grepl("B$", rowname))

lapply(stock_id, plot_horse, post_dat = post, stats_dat = summary)
lapply(stock_id, plot_rickeryear, stats_dat = summary)
profiles <- lapply(stock_id, get_profile, post_dat = post)
lapply(profiles, plot_profile)
lapply(profiles, plot_ey)
