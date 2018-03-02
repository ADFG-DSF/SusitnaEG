library(SusitnaEG)
dat <- age[grepl("^Deshka", age$location), ] %>%
  dplyr::mutate(x6 = x6 + x78) %>%
  dplyr::select(-x78)
X <- as.matrix(dat[, grep("^x.*", x = colnames(dat), value = TRUE)])

jags_dat <- list(K = sum(grepl("^x.*", x = colnames(dat))),
                 Y = length(unique(dat$year)),
                 one = rep(1 / sum(grepl("^x.*", x = colnames(dat))), sum(grepl("^x.*", x = colnames(dat)))),
                 X_m = X,
                 Xsum = ifelse(is.na(dat$n), 100, dat$n),
                 X_p = X
                 )

#paramaters of interest
params <- c("q_m", "q_p")

#MCMC settings
nc <- 3; nb <- 1000; nt <- 1; ns <- 5000

post <- jagsUI::jags(data = jags_dat,
                     parameters.to.save = params,
                     model.file = ".\\models\\mod_Poisson_multi_eq.r",
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
