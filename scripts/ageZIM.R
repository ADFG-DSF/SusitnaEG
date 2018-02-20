year = structure(1:length(unique(age$year)), names = unique(age$year))[age$year]
jags_dat <- list(K = sum(grepl("^x.*", x = colnames(age))),
                 I = length(unique(age$year)),
                 year = year,
                 year_pred = year["1979"]:max(year),
                 N_pred = sum(unique(year) >= year["1979"]),
                 J = length(unique(age$location)),
                 sample = structure(1:length(unique(age$location)), names = sort(unique(age$location)))[age$location],
                 N = dim(age)[1],
                 X = as.matrix(age[, grep("^x.*", x = colnames(age), value = TRUE)]),
                 Xsum = age$n
)

#paramaters of interest
params <- c("p0", "p_pred", "alpha", "beta", "gamma", "mu_beta", "sigma_beta", "mu_gamma", "sigma_gamma")

#MCMC settings
nc <- 3; nb <- 1000; nt <- 15; ns <- 31000

post <- jagsUI::jags(data = jags_dat,
                     parameters.to.save = params,
                     model.file = ".\\models\\mod_ageZIM.r",
                     n.chains = nc,
                     n.iter = ns,
                     n.burnin = nb,
                     n.thin = nt,
                     store.data = TRUE)

post[["summary"]][grep("^gamma", rownames(post$summary)), ]
post[["summary"]][grep("beta", rownames(post$summary)), ]
post[["summary"]][grep(("alpha"), rownames(post$summary)), ]
post[["summary"]][grep(("mu_"), rownames(post$summary)), ]
post[["summary"]][grep(("sigma_"), rownames(post$summary)), ]
post[["summary"]][grep(("p0\\[1,1"), rownames(post$summary)), ]
data.frame(param = names(post$summary[, "mean"]), est = post$summary[, "mean"]) %>%
  dplyr::filter(grepl("p_pred\\[", x = param)) %>%
  dplyr::mutate(year = gsub("^p_pred\\[(\\d*),\\d\\]", "\\1", param),
                age = gsub("^p_pred\\[\\d*,(\\d)\\]", "\\1", param),
                est = round(est, 3)) %>%
  dplyr::select(-param) %>%
  tidyr::spread(age, est) %>% 
  dplyr::arrange(as.numeric(year))

mu_gamma <- 
data.frame(param = names(post$summary[, "mean"]), mu_gamma = post$summary[, "mean"]) %>%
  dplyr::filter(grepl("^mu_gamma", x = param)) %>%
  dplyr::mutate(age = as.numeric(gsub("^mu_gamma\\[(\\d)\\]", "\\1", param)) + 2)
beta <-
data.frame(param = names(post$summary[, "mean"]), beta = post$summary[, "mean"]) %>%
  dplyr::filter(grepl("^beta\\[", x = param)) %>%
  dplyr::mutate(year = names(structure(1:length(unique(age$year)), names = unique(age$year))[as.numeric(gsub("^beta\\[(\\d*),\\d\\]", "\\1", param))]),
                age = as.numeric(gsub("^beta\\[\\d*,(\\d)\\]", "\\1", param)) + 2)

one <- 
dplyr::left_join(beta, mu_gamma, "age") %>%
  dplyr::mutate(mu = exp(beta + mu_gamma),
                deshka = exp(beta))

two <- one %>% dplyr::group_by(year) %>% dplyr::summarise(sum_mu = sum(mu), sum_deshka = sum(deshka))
dat <-
  age %>%
  dplyr::mutate(n = x3 + x4 + x5 + x6 + x78,
                p3 = round(x3 / n, 2),
                p4 = round(x4 / n, 2),
                p5 = round(x5 / n, 2),
                p6 = round(x6 / n, 2),
                p78 = round(x78 / n, 2)) %>%
  dplyr::select(year, location, dplyr::starts_with("p")) %>%
  tidyr::gather(age, prop, -year, - location)
dplyr::left_join(one, two, "year") %>%
  dplyr::mutate(p_mu = mu / (1 + sum_mu),
                p_deshka = deshka / (1 + sum_deshka)) %>%
  dplyr::arrange(year) %>%
  tidyr::gather(group, p, p_mu, p_deshka) %>%
  dplyr::select(year, age, group, prop = p) %>%
  dplyr::mutate(age = ifelse(age < 7, paste0("p", age), "p78")) %>%
  ggplot(aes(x = as.numeric(year), y = prop, color = group)) + 
    geom_line() + 
    geom_point(data = dat, aes(color = location)) +
    facet_grid(age ~ .)

