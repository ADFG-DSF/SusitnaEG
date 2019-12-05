packs <- c("SusitnaEG", "jagsUI")
lapply(packs, require, character.only = TRUE)

rm(list=ls(all=TRUE))

get_ids(year_range = 1979:2019)

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
#2019 MR is prelim so read in here.
mr[[1]][41,] <- c(8071, 15475, 7400, NA)
mr[[2]][41,] <- c(3173/8071, 4209/15475, 2937/7400, 0.1)

####  Bundle data to be passed to JAGS  ####
dat = list(
  Y = length(year_id), A = ncol(x.a), SG = length(stock_id), I = sum(sapply(trib_id, function(x) {length(x[!grepl("Other", x)])})),
  a.min = age_min, a.max = age_max, 
  x.a = x.a, n.a = rowSums(x.a), yr.a = a$yr.a, J = length(a$yr.a), x.stock = a$stock, 
  tele.S2 = telemetry$'East Susitna', tele.S3 = telemetry$Talkeetna, tele.S4 = telemetry$Yentna,
  Ntele.S2 = telemetry$'N_East Susitna', Ntele.S3 = telemetry$N_Talkeetna, Ntele.S4 = telemetry$N_Yentna,
  air.S1 = as.vector(as[[1]]), air.S2 = as[[2]], air.S3 = as[[3]], air.S4 = as[[4]],
  Hm.hat = Hm$H, cv.Hm = Hm$cv,
  Ha.hat = Ha$Ha, cv.Ha = Ha$Ha_cv,
  Hd.hat = Hd$H, cv.Hd = Hd$cv,
  MR = mr[[1]], cv.MR = mr[[2]],
  weir = weir,
  small3 = rbind(matrix(0, length(year_id) - sum(lt500$age == "1.1"), 2), as.matrix(lt500[lt500$age == "1.1", c("n_small", "n")])),
  small4 = rbind(matrix(0, length(year_id) - sum(lt500$age == "1.2"), 2), as.matrix(lt500[lt500$age == "1.2", c("n_small", "n")]))
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
'mu.Hmarine', 'mu.Habove', 'p.HDeshka', 'Bsum.HDeshka', 'HDeshka', 'IR_deshka'
)

#MCMC settings
nc <- 3
nb <- 50000
nt <- 200
ns <- 200000

#MCMC settings
nc <- 3
nb <- 5000
nt <- 100
ns <- 20000

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

saveRDS(post, file = ".\\posts\\SuChinook_adddata_fb451a7.rds")
post <- readRDS(".\\posts\\SuChinook_adddata_fb451a7.rds")

rhat <- get_Rhat(post, cutoff = 1.15)
rhat
#lapply(rownames(rhat[[1]][rhat[[1]]$Rhat >= quantile(rhat[[1]]$Rhat, .9), , drop = FALSE]), jagsUI::traceplot, x = post)
lapply(rownames(rhat[[1]]), jagsUI::traceplot, x = post)
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
post$summary["B", ]

#model fit plots
lapply(stock_id, plot_fit, post_dat = post)

#state varible plots
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

#Jan23 UCI meeting proposal
goals_df <- data.frame(stock = stock_id, 
                       lb = c(9000, 13000, 9000, 13000), 
                       ub = c(18000, 25000, 17500, 22000))
goals_list <- split(goals_df[, -1], 1:nrow(goals_df))

#Profiles
mapply(plot_profile, profile_dat = profiles, goal_range = goals_list, SIMPLIFY = FALSE)

#expected yield
mapply(plot_ey, profile_dat = profiles, goal_range = goals_list, SIMPLIFY = FALSE)

#escapement vrs. proposed goals
plot_Swgoals(post, goals_df)
