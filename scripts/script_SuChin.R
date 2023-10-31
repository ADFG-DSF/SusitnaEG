packs <- c("jagsUI", "tidyverse")
lapply(packs, require, character.only = TRUE)

rm(list=ls(all=TRUE))
source(".\\functions\\get.R")
source(".\\functions\\internal.R")
source(".\\functions\\plot.R")
source(".\\functions\\table.R")
data_names <- list.files(path=".\\data")
lapply(data_names, function(x) load(paste0(".\\data\\", x), .GlobalEnv))

get_ids()

#recall get_Hhat()

a <- 
  age %>%
  dplyr::rowwise() %>%
  dplyr::mutate(stock = which(stock_id == stock)) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(data.frame(yr.a = as.numeric(names(year_id)), year = year_id, stringsAsFactors = FALSE),
                   by = "year") %>%
  dplyr::select(yr.a, stock, x3, x4, x5, x678) %>%
  tidyr::gather(age, n, -yr.a, -stock) %>%
  dplyr::group_by(yr.a, stock, age) %>%
  dplyr::summarise(n = sum(n)) %>%
  tidyr::spread(age, n)
table(a$yr.a, a$stock)
x.a <- as.matrix(a[, grepl("x", names(a))])

####  Bundle data to be passed to JAGS  ####
dat = list(
  Y = length(year_id), A = ncol(x.a), SG = length(stock_id), I = sum(sapply(trib_id, function(x) {length(x[!grepl("Other", x)])})),
  a.min = age_min, a.max = age_max, 
  x.a = x.a, n.a = rowSums(x.a), yr.a = a$yr.a, J = length(a$yr.a), x.stock = a$stock, 
  tele.S2 = telemetry$'East Susitna', tele.S3 = telemetry$Talkeetna, tele.S4 = telemetry$Yentna,
  Ntele.S2 = telemetry$'N_East Susitna', Ntele.S3 = telemetry$N_Talkeetna, Ntele.S4 = telemetry$N_Yentna,
  air.S1 = as.vector(as[[1]]), air.S2 = as[[2]], air.S3 = as[[3]], air.S4 = as[[4]],
  Hm.hat = c(Hm$H), cv.Hm = Hm$cv,
  Ha.hat = Ha$Ha, cv.Ha = Ha$Ha_cv,
  Hd.hat = Hd$H, cv.Hd = Hd$cv,
  MR = mr[[1]], cv.MR = mr[[2]],
  weir = weir,
  small3 = rbind(matrix(0, length(year_id) - sum(lt500$age == "1.1"), 2), as.matrix(lt500[lt500$age == "1.1", c("n_small", "n")])),
  small4 = rbind(matrix(0, length(year_id) - sum(lt500$age == "1.2"), 2), as.matrix(lt500[lt500$age == "1.2", c("n_small", "n")])),
  MR_det = mr[[3]], 
  tau.logMR_det = mr[[4]], #2018 MR for stocks 1:3
  tele.p2upS4 = sonar[[2]], #Lake Creek telemetry and Sonar
  sonar = sonar[[1]]
)

####  Define the parameters (nodes) of interest  ##### 
parameters=c(
'sigma.white', 'sigma.R0', 'sigma.air', 'C_as', 'sigma.weir',
'beta', 'lnalpha', 'mu.lnalpha', 'sigma.lnalpha', 'lnalpha.c', 'alpha', 'lnalpha.vec', 
'phi', 'log.resid.0', 'log.resid.vec',
'S.eq', 'S.max', 'S.msy', 'U.msy',
'p', 'pi', 'Dsum.age', 'ML1', 'ML2',
'S','N','R','IR',
'N.ta','q', 'b', 'q.star', 'N.tas',
'Dsum.S2', 'ML1.S2', 'ML2.S2', 'Dsum.S3', 'ML1.S3', 'ML2.S3', 'Dsum.S4', 'ML1.S4', 'ML2.S4',
'p.S2', 'p.S3', 'p.S4', 'Bsum.So',
'theta',
'p.small3', 'p.small4', 
'mu.Hmarine', 'mu.Habove', 'p.HDeshka', 'Bsum.HDeshka', 'HDeshka', 'IR_deshka',
"p.p2upS4", "log.1p2upS4"
)

#MCMC settings
nc <- 3
nb <- 250000
nt <- 500
ns <- 1000000

#MCMC settings
# nc <- 3
# nb <- 10000
# nt <- 50
# ns <- 50000


post <- jags(data = dat,
             parameters.to.save = parameters,
             inits = get_inits,
             model.file = ".\\models\\mod_SuChin.r",
             n.chains = nc,
             n.iter = ns,
             n.burnin = nb,
             n.thin = nt,
             parallel = TRUE,
             store.data = TRUE
)

#saveRDS(post, file = ".\\posts\\SuChinook_10242023.rds") #Associate this date w the appropriate commit.
post <- readRDS(".\\posts\\SuChinook_10242023.rds")

#note: convergence is poor for age 3 fish.
rhat <- get_Rhat(post, cutoff = 1.15)
rhat
jagsUI::traceplot(post, Rhat_min = 1.15)

#inspect convergence
#shinystan::launch_shinystan(shinystan::as.shinystan(post))

#Drainage wide age composition
table_age(post, "p") #age-at-maturity
table_age(post, "q") #age comp
table_age(post, "N.ta") #total run by age

#age comp by stock
post$summary[grepl("b\\[", rownames(post$summary)), ]
#age at maturity trend maintained
post$summary[grepl("ML2\\[", rownames(post$summary)), ]
plot_age(post)

#Stock Composition estimates
plot_stock(telemetry, post)
#trends in stock composition
post$summary[grepl("ML2\\.", rownames(post$summary)), ]
table_stock(post)

#survey detectability
plot_theta(post)
#survey variability
table_airerror(post)
#mean survey variability
post$summary["C_as", ]

#model fit plots
lapply(stock_id, plot_fit, post_dat = post)

#state variable plots
lapply(stock_id, function(x) plot_state(post, display = x, rp = c("msy", "msr")))
table_state(post, "bystock")
plot_statepairs(post, plot = "bystock")

#SR relationships
lapply(stock_id, plot_horse, post_dat = post)
lapply(stock_id, plot_rickeryear, post_dat = post)

#SR parameters
table_params(post)

#create profile dataset
profiles <- lapply(stock_id, get_profile, post_dat = post)

#current goals
goals_df <- data.frame(stock = stock_id, 
                       lb = c(9000, 13000, 9000, 16000), 
                       ub = c(18000, 25000, 17500, 22000))
goals_list <- split(goals_df[, -1], 1:nrow(goals_df), ) 


#Profiles
mapply(plot_profile, profile_dat = profiles, goal_range = goals_list, SIMPLIFY = FALSE)


post_BOF2020 <- readRDS(".\\posts\\SuChinook_134cf92.rds")
profiles_BOF2020 <- lapply(stock_id, get_profile, post_dat = post_BOF2020)
list_BOF2020 = Map(function(x,y,z){list(x, y, z, "2020 BOF")}, 
               profiles_BOF2020,
               goals_list, 
               post$q50$S.msy)

list_update2023 = Map(function(x,y,z){list(x, unname(y), z, "2023 update")}, 
               profiles,
               goals_list, 
               post$q50$S.msy) 
Map(plot_OYPcompare, list_BOF2020, list_update2023, plotmax = list(30000, 30000, 25000, 30000))

#expected yield
mapply(plot_ey, profile_dat = profiles, goal_range = goals_list, SIMPLIFY = FALSE)

#escapement vrs. proposed goals
plot_Swgoals(post, goals_df)


# Note the 2022 Lake Creek Sonar estimate was revised upwards (4,231 vrs 3,999) while the 2022 Yentna escapement estimate fell after 2023 Lake Creek was added 
# (and 2022 Lake Creek sonar was revised) from 16,583 to 15,407. In general, sonar data has put upwards pressure on the escapement estimate relative to the survey data.
post_2022 <- readRDS(".\\posts\\SuChinook_11302022.rds")
post_2023 <- readRDS(".\\posts\\SuChinook_10242023.rds")

#sonar point estimates
dat$sonar[44:45] / post_2023$q50$p.p2upS4[44:45] / post_2023$q50$p.S4[44:45, 2]
#survey point estimates
dat$air.S4[44:45, ] / t(post_2023$q50$theta[10:13, 44:45]) / post_2023$q50$p.S4[44:45, 1:4]

#The model response to this pressure has been to decrease the estimate of Lake Creek's contribution to the Yentna Stock
# Both as a mean response
#### Note: seeing the model in this form makes me wonder about including variability in the estimate of the non-surveyed proportion. 
#### One model simplification would be to make this a constant scalar
plot_meanstockcomp <- function( ML1, ML2, p_out, pop_names, years){
  logistic <- p_mean1 <- p_mean2 <- matrix(NA, nrow = length(years), ncol = length(pop_names))
  
  for(y in years){
    for(p in 1:length(pop_names)){
      logistic[y, p] <- exp(ML1[p] + ML2[p] * y)
    }
  }
  for(y in years){
    for(p in 1:length(pop_names)){
      p_mean1[y, p] <- logistic[y, p] / sum(logistic[y, ])
      p_mean2[y, p] <- p_mean1[y, p] * (1 - p_out[y])
    }
  }
  colnames(p_mean2) <- pop_names
  p_mean2 %>% as.data.frame() %>% rownames_to_column() %>% pivot_longer(-rowname)
}
post_2021 <- readRDS(".\\posts\\SuChinook_2021_MR.rds")
post_2020 <- readRDS(".\\posts\\SuChinook_Bparam_2sf0a9c2.rds")
plot_meanstockcomp(post_2022$q50$ML1.S4, post_2022$q50$ML2.S4, post_2022$q50$p.S4[, 5], trib_id$Yentna[1:4], 1:44) %>%
  mutate(post = "2022") %>%
  rbind(plot_meanstockcomp(post_2023$q50$ML1.S4, post_2023$q50$ML2.S4, post_2023$q50$p.S4[, 5], trib_id$Yentna[1:4], 1:45) %>% mutate(post = "2023")) %>%
  rbind(plot_meanstockcomp(post_2021$q50$ML1.S4, post_2021$q50$ML2.S4, post_2021$q50$p.S4[, 5], trib_id$Yentna[1:4], 1:43) %>% mutate(post = "2021")) %>%
  rbind(plot_meanstockcomp(post_2020$q50$ML1.S4, post_2020$q50$ML2.S4, post_2020$q50$p.S4[, 5], trib_id$Yentna[1:4], 1:42) %>% mutate(post = "2020")) %>%
  ggplot(aes(x = as.numeric(rowname), y = value, color = post)) +
  geom_line() +
  facet_grid(name ~ .)

#and as a final estimate
get_comp <- function(post, pop_names, years){
  colnames(post) <- pop_names
  post %>% as.data.frame() %>% mutate(rowname = years) %>% pivot_longer(-rowname)
}
get_comp(post_2022$q50$p.S4[, 1:4], trib_id$Yentna[1:4], 1979:2022) %>%
  mutate(post = "2022") %>%
  rbind(get_comp(post_2023$q50$p.S4[, 1:4], trib_id$Yentna[1:4], 1979:2023) %>% mutate(post = "2023")) %>%
  rbind(get_comp(post_2021$q50$p.S4[, 1:4], trib_id$Yentna[1:4], 1979:2021) %>% mutate(post = "2021")) %>%
  rbind(get_comp(post_2020$q50$p.S4[, 1:4], trib_id$Yentna[1:4], 1979:2020) %>% mutate(post = "2020")) %>%
  ggplot(aes(x = as.numeric(rowname), y = value, color = post)) +
  geom_line() +
  facet_grid(name ~ .)

# General comment: since the model was published we have ran into several instances where the estimate of stock composition has moved to adjust to new data. In this case 
# I'd say the movement was in a desirable direction but there was an instance where it wasn't (see SusitnaEG_prelim2020.html). The stock composition part of this 
# model should be refined/simplified.