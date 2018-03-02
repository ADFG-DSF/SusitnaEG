library(SusitnaEG)
dat <- age[grepl("^Deshka", age$location), ] %>%
  dplyr::mutate(x6 = x6 + x78) %>%
  dplyr::select(-x78)
X <- as.matrix(dat[, grep("^x.*", x = colnames(dat), value = TRUE)])
X_pmiss <- X
X_pmiss[, "x3"] <- ifelse(X_pmiss[, "x3"] /  age$n[grepl("^Deshka", age$location)] <= 0.02, NA, X_pmiss[, "x3"])
  #ifelse(X_pmiss[, "x3"] == 0, NA, X_pmiss[, "x3"])
  #ifelse(grepl(".*creel$", dat$location), NA, X_pmiss[, "x3"])
dat_all <- age[age$year >= 1979, ] %>%
  dplyr::mutate(x6 = x6 + x78) %>%
  dplyr::filter(!grepl("Escapement$|Flathorn", location)) %>%
  dplyr::select(-x78)
dat_all$year <- as.numeric(dat_all$year) - min(as.numeric(dat_all$year)) + 1
dat_missall <- dat_all
dat_missall$x3 <- ifelse(dat_all$x3 / dat_all$n <= 0.02, NA, dat_all$x3)
  #ifelse(dat_missall$x3 == 0 & (grepl("^Deshka", dat_missall$location) | grepl("^Alexander", dat_missall$location)), NA, dat_missall$x3)
  #ifelse(grepl(".*creel$", dat_all$location), NA, dat_missall$x3)



jags_dat <- list(K = sum(grepl("^x.*", x = colnames(dat))),
                 Y = length(unique(dat$year)),
                 one = rep(1 / sum(grepl("^x.*", x = colnames(dat))), sum(grepl("^x.*", x = colnames(dat)))),
                 X_m = X,
                 Xsum = ifelse(is.na(dat$n), 100, dat$n),
                 X_p = X,
                 X_pmiss = X_pmiss,
                 N = dim(dat_all)[1],
                 year = dat_all$year,
                 X_pall = as.matrix(dat_all[, grep("^x.*", x = colnames(dat_all), value = TRUE)]),
                 X_pmissall = as.matrix(dat_missall[, grep("^x.*", x = colnames(dat_missall), value = TRUE)])
                 )

#paramaters of interest
params <- c("q_m", "q_p", "q_pmiss", "q_pall", "q_pmissall")

#MCMC settings
nc <- 3; nb <- 1000; nt <- 1; ns <- 5000

post <- jagsUI::jags(data = jags_dat,
                     parameters.to.save = params,
                     model.file = ".\\models\\mod_agePoisson.r",
                     n.chains = nc,
                     n.iter = ns,
                     n.burnin = nb,
                     n.thin = nt,
                     store.data = TRUE)
post

q_m <- post[["summary"]][grep("q_m", rownames(post$summary)), ]
q_p <- post[["summary"]][grep("q_p\\[", rownames(post$summary)), ]
diff <- q_m[, "mean"] - q_p[, "mean"] #multinomial and posiion likelihoods give similar estimates for q
plot(diff)
#differences were in the missing year
post[["summary"]][grep("q_m\\[12", rownames(post$summary)), ]
post[["summary"]][grep("q_p\\[12", rownames(post$summary)), ]

#data to plot
plot_dat <-
  age %>%
  dplyr::filter(year >= "1979") %>%
  dplyr::mutate(n = ifelse(is.na(x3 + x4 + x5 + x6 + x78), x4 + x5 + x6 + x78, x3 + x4 + x5 + x6 + x78),
                p3 = round(x3 / n, 2),
                p4 = round(x4 / n, 2),
                p5 = round(x5 / n, 2),
                p6 = round((x6 + x78) / n, 2),
                year = as.numeric(year) - min(as.numeric(year)) + 1) %>%
  dplyr::select(year, location, n, dplyr::starts_with("p")) %>%
  tidyr::gather(age, p, -year, -n, -location) %>%
  dplyr::mutate(sample = ifelse((p <= 0.02 & age =="p3") | grepl("Escapement$|Flathorn", location), "age3==NA", "age3==data"))

#estimates of q from Poisson regression with extra data
q_p <- 
  data.frame(param = names(post$summary[, "mean"]), p = post$summary[, "mean"]) %>%
  dplyr::filter(grepl("^q_p\\[", x = param)) %>%
  dplyr::mutate(year = as.numeric(gsub("^q_p\\[(\\d*),\\d\\]", "\\1", param)),
                age = paste0("p", as.numeric(gsub("^q_p\\[\\d*,(\\d)\\]", "\\1", param)) + 2)) 

#estimates of q from Poisson regression with missing and extra data
q_pall <- 
  data.frame(param = names(post$summary[, "mean"]), p = post$summary[, "mean"]) %>%
    dplyr::filter(grepl("^q_pall", x = param)) %>%
    dplyr::mutate(year = as.numeric(gsub("^q_pall\\[(\\d*),\\d\\]", "\\1", param)),
                age = paste0("p", as.numeric(gsub("^q_pall\\[\\d*,(\\d)\\]", "\\1", param)) + 2)) 

#estimates of q from Poisson regression with missing and extra data
q_pmissall <- 
  data.frame(param = names(post$summary[, "mean"]), p = post$summary[, "mean"]) %>%
  dplyr::filter(grepl("^q_pmissall", x = param)) %>%
  dplyr::mutate(year = as.numeric(gsub("^q_pmissall\\[(\\d*),\\d\\]", "\\1", param)),
                age = paste0("p", as.numeric(gsub("^q_pmissall\\[\\d*,(\\d)\\]", "\\1", param)) + 2)) 

library(ggplot2)
#Red line = no missing data, deshka data only
#Black line missing age3 data, deshka data only
#Blue line no missing data, all availible data 
#Orange line missing age3 data, all availible data 
data.frame(param = names(post$summary[, "mean"]), p = post$summary[, "mean"]) %>%
  dplyr::filter(grepl("^q_pmiss\\[", x = param)) %>%
  dplyr::mutate(year = as.numeric(gsub("^q_pmiss\\[(\\d*),\\d\\]", "\\1", param)),
                age = paste0("p", as.numeric(gsub("^q_pmiss\\[\\d*,(\\d)\\]", "\\1", param)) + 2)) %>%
  ggplot(aes(x = year, y = p)) + #plot it
    geom_line() + 
    geom_line(data = q_p, color = "red") +
#    geom_line(data = q_pall, color = "blue") +
    geom_line(data = q_pmissall, color = "orange") +
    geom_point(data = plot_dat, aes(size = n, color = sample)) +
    facet_grid(age ~ .)

data.frame(deshka = post[["summary"]][grep("q_p\\[\\d*,1", rownames(post$summary)), ][, "mean"],
           deshka_miss = post[["summary"]][grep("q_pmiss\\[\\d*,1", rownames(post$summary)), ][, "mean"],
           all = post[["summary"]][grep("q_pall\\[\\d*,1", rownames(post$summary)), ][, "mean"],
           all_miss = post[["summary"]][grep("q_pmissall\\[\\d*,1", rownames(post$summary)), ][, "mean"]) %>%
  round(2)
  

