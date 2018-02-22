library(SusitnaEG)
dat <- age[grepl("^Deshka", age$location), ]
X <- as.matrix(dat[, grep("^x.*", x = colnames(dat), value = TRUE)])
X_pmiss <- X
X_pmiss[, "x3"] <- ifelse(X_pmiss[, "x3"] /  age$n[grepl("^Deshka", age$location)] <= 0.02, NA, X_pmiss[, "x3"])
dat2 <- age[age$year >= 1979, ]
dat2 <- dat2[!grepl("^Alexander", dat2$location), ]
dat2$x3 <- ifelse(dat2$x3 / dat2$n <= 0.02 & grepl("^Deshka", dat2$location), NA, dat2$x3)
dat2$year <- as.numeric(dat2$year) - min(as.numeric(dat2$year)) + 1


jags_dat <- list(K = sum(grepl("^x.*", x = colnames(dat))),
                 Y = length(unique(dat$year)),
                 one = rep(1 / sum(grepl("^x.*", x = colnames(dat))), sum(grepl("^x.*", x = colnames(dat)))),
                 X_m = X,
                 Xsum = ifelse(is.na(dat$n), 100, dat$n),
                 X_p = X,
                 X_pmiss = X_pmiss,
                 N = dim(dat2)[1],
                 X_pall = as.matrix(dat2[, grep("^x.*", x = colnames(dat2), value = TRUE)]),
                 year = dat2$year)

#paramaters of interest
params <- c("q_m", "q_p", "q_pmiss", "q_pall")

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
plot(diff)
diff <- q_m[, "mean"] - q_p[, "mean"] #multinomial and posiion likelihoods give similar estimates for q
#differences were in the missing year
post[["summary"]][grep("q_m\\[12", rownames(post$summary)), ]
post[["summary"]][grep("q_p\\[12", rownames(post$summary)), ]

#data to plot
plot_dat <-
  dat2 %>%
  dplyr::mutate(n = ifelse(is.na(x3 + x4 + x5 + x6 + x78), x4 + x5 + x6 + x78, x3 + x4 + x5 + x6 + x78),
                p3 = round(x3 / n, 2),
                p4 = round(x4 / n, 2),
                p5 = round(x5 / n, 2),
                p6 = round(x6 / n, 2),
                p7 = round(x78 / n, 2),
                year = as.numeric(year) - min(as.numeric(year)) + 1,
                sample = ifelse(grepl("^Deshka", location), "Deshka", "Other")) %>%
  dplyr::select(year, sample, n, dplyr::starts_with("p")) %>%
  tidyr::gather(age, p, -year, -n, -sample)

#estimates of q from Poisson regression with missing and extra data
plot_dat2 <- 
  data.frame(param = names(post$summary[, "mean"]), p = post$summary[, "mean"]) %>%
    dplyr::filter(grepl("^q_pall", x = param)) %>%
    dplyr::mutate(year = as.numeric(gsub("^q_pall\\[(\\d*),\\d\\]", "\\1", param)),
                age = paste0("p", as.numeric(gsub("^q_pall\\[\\d*,(\\d)\\]", "\\1", param)) + 2)) 

library(ggplot2)
#both lines model missing age 3 fish
#red line estimated q with all availible data, black line only deshka data 
data.frame(param = names(post$summary[, "mean"]), p = post$summary[, "mean"]) %>%
  dplyr::filter(grepl("^q_pmiss", x = param)) %>%
  dplyr::mutate(year = as.numeric(gsub("^q_pmiss\\[(\\d*),\\d\\]", "\\1", param)),
                age = paste0("p", as.numeric(gsub("^q_pmiss\\[\\d*,(\\d)\\]", "\\1", param)) + 2)) %>%
  ggplot(aes(x = year, y = p)) + #plot it
    geom_line() + geom_line(data = plot_dat2, color = "red") +
    geom_point(data = plot_dat, aes(size = n, color = sample)) +
    facet_grid(age ~ .)

