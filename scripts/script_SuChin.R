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

Ha.hat0 <- 
  data.frame(C = Ha$C, 
             E = Ha$E, 
             F = Ha$F, 
             Y = rowSums(Ha[, names(Ha) %in% c("J", "K", "L", "N")], na.rm = TRUE), 
             R = rowSums(Ha[, names(Ha) %in% c("B", "G", "H")], na.rm = TRUE)) %>%
  dplyr::mutate_all(function(x){ifelse(x == 0, 1, x)}) %>%
  as.matrix()
Ha.hat <- Ha.hat0 %>% rbind(matrix(NA, nrow = length(year_id) - dim(Ha.hat0)[1], ncol = dim(Ha.hat0)[2]))

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
  tele.S2 = telemetry$'East Susitna', Ntele.S2 = telemetry$'N_East Susitna',
  tele.S3 = telemetry$Talkeetna, Ntele.S3 = telemetry$N_Talkeetna,
  tele.S4 = telemetry$Yentna, Ntele.S4 = telemetry$N_Yentna,
  tele.S5 = telemetry$Other, Ntele.S5 = telemetry$N_Other,
  air.S1 = as.vector(as[[1]]), 
  air.S2 = as[[2]],
  air.S3 = as[[3]],
  air.S4 = as[[4]],
  air.S5 = as[[5]], 
  Hm.hat = c(Hm$Hm_Susitna, rep(NA, length(year_id) - length(Hm$Hm_Susitna))), cv.Hm = rep(0.05, length(year_id)),
  Ha.hat = Ha.hat, cv.Ha = rep(0.2, dim(Ha.hat)[1]),
  MR = mr, 
  cv.MR = rep(0.15, dim(mr)[1]),
  weir = weir,
  small3 = rbind(matrix(0, length(year_id) - sum(lt500$age == "1.1"), 2), as.matrix(lt500[lt500$age == "1.1", c("n_small", "n")])),
  small4 = rbind(matrix(0, length(year_id) - sum(lt500$age == "1.2"), 2), as.matrix(lt500[lt500$age == "1.2", c("n_small", "n")]))
)

# bundle inits for JAGS
inits <- list(get_inits(), get_inits())

####  Define the parameters (nodes) of interest  ##### 
parameters=c(
'sigma.white', 'sigma.R0', 'sigma.air', 'sigma.weir',
'beta', 'mu.beta', 'sigma.beta', 'lnalpha', 'mu.lnalpha', 'sigma.lnalpha', 'lnalpha.c', 'alpha', 'lnalpha.vec', 
'phi', 'log.resid.0', 'log.resid.vec',
'S.eq', 'S.max', 'S.msy', 'U.msy',
'p', 'pi', 'Dsum.age', 'ML1', 'ML2',
'S','N','R','IR',
'N.ta','q', 'b', 'q.star',
'Dsum.S2', 'Dsum.S3', 'Dsum.S4', 'Dsum.S5',
'p.S2', 'p.S3', 'p.S4', 'p.S5',
'theta', 'Dsum.theta', 'ML1.theta', 'ML2.theta', "mu_ML1t", "mu_ML2t",
'p.small3', 'p.small4', 
'mu.Hmarine', 'mu.Habove'
)

#### run JAGS ####
ptm = proc.time()
jmod = jags.model(file=".\\models\\mod_SuChin.r", data=dat, n.chains=2, inits=inits, n.adapt=1000)  
update(jmod, n.iter=1000, by=1, progress.bar='text')               
post = coda.samples(jmod, parameters, n.iter=3000, thin=1)        # 10 min
update(jmod, n.iter=15000, by=1, progress.bar='text')               
post = coda.samples(jmod, parameters, n.iter=10000, thin=10)         
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

sapply(1:17, function(x){
  tibble::rownames_to_column(summary) %>% 
    dplyr::filter(grepl(paste0("^theta\\[", x, ",\\d+\\]"), rowname)) %>% 
    dplyr::select(Mean) %>% unlist()}
)

tibble::rownames_to_column(summary) %>% 
  dplyr::filter(grepl(paste0("^theta\\["), rowname)) %>% 
  dplyr::mutate(year = gsub("theta\\[\\d+,(\\d+)\\]", "\\1", rowname),
                trib = gsub("theta\\[(\\d+),\\d+\\]", "\\1", rowname)) %>%
  ggplot(aes(x = as.numeric(year), y = Mean, color = trib)) +
    geom_line()

lapply(1:5, function(x){
  tibble::rownames_to_column(summary) %>% 
    dplyr::filter(grepl(paste0("^mu.Habove\\[\\d+,", x, "\\]"), rowname)) %>% 
    dplyr::select(Mean) %>% 
    unlist() %>% 
    hist()}
  )

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

lapply(stock_id, plot_fit, stats_dat = summary)
lapply(1:5, function(x) plot_state(summary, stock = x, S_msr = TRUE))
plot_statepairs(post)

#2d age at maturity and age comp arrays
table_age(summary, "p") #age-at-maturity
table_age(summary, "q") #age comp
table_age(summary, "N.ta") #total run by age

plot_age(as.data.frame(x.a), summary)

table_stock(summary)
plot_stock(telemetry, summary)

plot_theta(summary)

lapply(stock_id, plot_horse, post_dat = post, stats_dat = summary)
lapply(stock_id, plot_rickeryear, stats_dat = summary)
profiles <- lapply(stock_id, get_profile, post_dat = post)
lapply(profiles, plot_profile)
lapply(profiles, plot_ey)
