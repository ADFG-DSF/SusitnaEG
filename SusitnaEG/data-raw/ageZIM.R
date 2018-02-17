age_p <-
readxl::read_excel(".\\SusitnaEG\\data-raw\\Susitna run reconstruction data_Jan102018.xlsx",
                   range = "Deshka brood table!I12:M50",
                   col_names = c("x3", "x4", "x5", "x6", "x7"))

age <- 
  sapply(age_p, function(x) as.integer(x * 100)) %>% #effective sample size of 100
  as.data.frame() %>%
  dplyr::mutate(year = as.character(1979:2017),
                location = "Deshka weir") %>%
  dplyr::rename(x78 = x7)

#devtools::use_data(age, pkg = ".\\SusitnaEG", overwrite = TRUE)

rawage_mark <-
  readxl::read_excel(".\\SusitnaEG\\data-raw\\comm fish data\\Copy of KING_CHUM_COHO_DATA.xlsx",
                     range = "King data!A2:P15650",
                     col_names = c("daycode", "year", "location_raw", "spp", "mefl", "sex", "age", "age_error"),
                     col_types = c(rep("text", 2), rep("skip", 3), "text", rep("skip", 4), "text", "numeric", "skip", rep("text", 3)),
                     skip = 1, 
                     na = "-999") 

lapply(rawage_mark, table, useNA = "ifany")
rawage_mark[is.na(rawage_mark$daycode), ] %>% print(n = 500) # keep
rawage_mark[is.na(rawage_mark$year), ] %>% print(n = 100) #drop
table(rawage_mark$location_raw, rawage_mark$year)
rawage_mark[is.na(rawage_mark$spp), ] %>% print(n = 100) #drop
rawage_mark[(rawage_mark$mefl <= 200) | (rawage_mark$mefl >= 1600) | is.na(rawage_mark$mefl), ] %>% print(n = 100) #make NA
rawage_mark[!(rawage_mark$sex %in% c( "1", "2")), ] %>% print(n = 100) #make NA
rawage_mark[!(rawage_mark$age %in% c( "11", "12", "13", "14", "15", "16")) & !is.na(rawage_mark$age), ] %>% print(n = 600) #keep 2., otherwise make NA
rawage_mark[(rawage_mark$age_error %in% c("12", "32")), ] %>% print(n = 100) #errors but ingnore
table(rawage_mark$age, rawage_mark$age_error)

age_mark <- 
  rawage_mark %>%
  dplyr::filter(!is.na(year),
                !is.na(spp)) %>%
  dplyr::mutate(location = ifelse(location_raw == "Susuitna river- Curry", "Susitna River-Curry", location_raw),
                location = ifelse(location == "Yentna River escapement", "Yentna River Escapement", location),
                mefl = ifelse((mefl <= 200) | (mefl >= 1600), NA, mefl),
                sex = ifelse(sex %in% c("1", "2"), sex, NA),
                age = ifelse(age %in% c( "11", "12", "13", "14", "15", "16", "21", "22", "23", "24"), age, NA)) %>%
  dplyr::filter(!is.na(age),
                grepl("Susitna|Yentna", location_raw)) %>%
  dplyr::mutate(age_f = forcats::fct_collapse(factor(age),
                                              "x3" = "11", "x4" = c("12", "21"), "x5" = c("13", "22"), "x6" = c("14", "23"), "x78" = c("15", "24", "16")))

lapply(age_mark, table, useNA = "ifany")
table(age_mark$location, age_mark$year)

rawage_mark2 <-
  readr::read_fwf(".\\SusitnaEG\\data-raw\\comm fish data\\MASTER_ASL_DATABASE_ARCHIVE_1967-2017.txt",
                  col_positions = readr::fwf_positions(c(26, 31, 67, 146, 160), c(29, 34, 77, 148, 163), c("daycode", "year", "stat_code", "spp", "age")),
                  col_types = "ccccc") %>%
  dplyr::filter(spp == "410",
                stat_code %in% c("24741100600", "24741100803", "24741100804", "24741100805", "24741100901", "24741101802", "24741103804")) %>%
  dplyr::mutate_all(function(x) ifelse(x == "-999", NA, x))

lapply(rawage_mark2, table, useNA = "ifany")
table(rawage_mark2$stat_code, rawage_mark2$year)
rawage_mark2[!(rawage_mark2$age %in% c( "11", "12", "13", "14", "15", "16")) & !is.na(rawage_mark2$age), ] %>% print(n = 600) #keep 2., otherwise make NA

age_mark2 <- 
  rawage_mark2 %>%
  dplyr::mutate(age = ifelse(age %in% c( "11", "12", "13", "14", "15", "16", "21", "22", "23", "24"), age, NA)) %>%
  dplyr::filter(!is.na(age)) %>%
  dplyr::mutate(age_f = forcats::fct_collapse(factor(age),
                                              "x3" = "11", "x4" = c("12", "21"), "x5" = c("13", "22"), "x6" = c("14", "23"), "x78" = c("15", "24", "16")),
                location = forcats::fct_collapse(factor(stat_code),
                                                 "Susitna River - Flathorn" = "24741100600",
                                                 "Susitna River Escapement" = c("24741100901", "24741103804"),
                                                 "Yentna River Escapement" = "24741101802"))

lapply(age_mark2, table, useNA = "ifany")
table(age_mark2$location, age_mark2$year)

age_comm <- rbind(age_mark[, c("daycode", "year", "age_f", "location")], age_mark2[, c("daycode", "year", "age_f", "location")])

id <-unique((age_comm[, c("year", "location")]))
xage <- 
  t(mapply(function(x, y){ 
  tab <- table(age_comm$age_f[age_comm$location == x & age_comm$year == y])},
  x = id[[2]],
  y = id[[1]],
  USE.NAMES = FALSE)) %>%
  as.data.frame() %>%
  dplyr::mutate(year = id[[1]],
                location = id[[2]]) %>%
  dplyr::select(year, location, dplyr::everything()) %>%
  dplyr::bind_rows(age) %>%
  dplyr::mutate(n = x3 + x4 + x5 + x6 + x78,
                p3 = round(x3 / n, 2),
                p4 = round(x4 / n, 2),
                p5 = round(x5 / n, 2),
                p6 = round(x6 / n, 2),
                p78 = round(x78 / n, 2)) %>%
  dplyr::arrange(year, location) 
#%>% dplyr::filter( year >= 1979)

library(ggplot2)
xage %>% 
  dplyr::select(year, location, dplyr::starts_with("p")) %>%
  tidyr::gather(age, prop, -year, - location) %>%
  ggplot(aes(x = year, y = prop, color = location)) + geom_point() + facet_grid(age ~ ., scales = "free_y")

year = structure(1:length(unique(xage$year)), names = unique(xage$year))[xage$year]
jags_dat <- list(K = sum(grepl("^x.*", x = colnames(xage))),
                 I = length(unique(xage$year)),
                 year = year,
                 year_pred = year["1979"]:max(year),
                 N_pred = sum(unique(year) >= year["1979"]),
                 J = length(unique(xage$location)),
                 sample = structure(1:length(unique(xage$location)), names = sort(unique(xage$location)))[xage$location],
                 N = dim(xage)[1],
                 X = as.matrix(xage[, grep("^x.*", x = colnames(xage), value = TRUE)]),
                 Xsum = xage$n
                )

#paramaters of interest
params <- c("p0", "p_pred", "alpha", "beta", "gamma", "mu_beta", "sigma_beta", "mu_gamma", "sigma_gamma")

#MCMC settings
nc <- 3; nb <- 1000; nt <- 15; ns <- 31000

post <- jagsUI::jags(data = jags_dat,
             parameters.to.save = params,
             model.file = ".\\models\\mod_agecomp4.r",
             n.chains = nc,
             n.iter = ns,
             n.burnin = nb,
             n.thin = nt,
             store.data = TRUE)

post[["summary"]][grep("gamma", rownames(post$summary)), ]
post[["summary"]][grep("beta", rownames(post$summary)), ]
post[["summary"]][grep(("alpha"), rownames(post$summary)), ]
post[["summary"]][grep(("mu_"), rownames(post$summary)), ]
post[["summary"]][grep(("sigma_"), rownames(post$summary)), ]
post[["summary"]][grep(("p0\\["), rownames(post$summary)), ]
data.frame(param = names(post$summary[, "mean"]), est = post$summary[, "mean"]) %>%
  dplyr::filter(grepl("p_pred\\[", x = param)) %>%
  dplyr::mutate(year = gsub("^p_pred\\[(\\d*),\\d\\]", "\\1", param),
                age = gsub("^p_pred\\[\\d*,(\\d)\\]", "\\1", param),
                est = round(est, 3)) %>%
  dplyr::select(-param) %>%
  tidyr::spread(age, est) %>% 
  dplyr::arrange(as.numeric(year))

data.frame(param = names(post$summary[, "mean"]), est = post$summary[, "mean"]) %>%
  dplyr::filter(grepl("p\\[", x = param)) %>%
  dplyr::mutate(year = rep(names(year), times = jags_dat$K),
                sample = rep(names(structure(1:length(unique(xage$location)), names = unique(xage$location))[xage$location]), times = jags_dat$K),
                age = gsub("^p\\[\\d*,(\\d)\\]", "\\1", param),
                est = round(est, 3)) %>%
  dplyr::select(-param) %>%
  tidyr::spread(age, est)
