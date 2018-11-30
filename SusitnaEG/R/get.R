#' Creates a dataset for plotting OYP, ORP and EY plots based on aerial count data
#'
#' This function creates a dataframe that can be used by plot_countprofile()
#'
#' @param post_dat An mcmc object with nodes lnalpha, beta, S.msy, lnalpha.c, p.S#, theta, and sigma.air from which you want 
#' to simulate SR relationships.
#' @param stock_name A character element specifying the stock of interest.
#' @param trib_name A character element specifying the tributary aerial data.
#'
#' @return A data.frame
#'
#' @examples
#' get_ids()
#' profile <- get_countprofile(post, "East Susitna", "Willow")
#'
#' @export
get_countprofile <- function(post_dat, stock_name, trib_name){
  stopifnot(exists("stock_id", .GlobalEnv),
            exists("trib_id", .GlobalEnv),
            "package:SusitnaEG" %in% search())
  
  trib_id0 <- lapply(1:5, function(x) trib_id[[x]][!grepl("Other", trib_id[[x]])])
  id <- data.frame(stock = factor(rep(names(trib_id), sapply(trib_id0, length)), levels = stock_id), 
                   tribn = unlist(sapply(trib_id0, function(x) 1:length(x)), use.names = FALSE), 
                   tribn2 = 1:length(unlist(trib_id0)),
                   trib = unlist(trib_id0, use.names = FALSE),
                   stringsAsFactors = FALSE) %>%
    dplyr::arrange(stock, tribn, trib)
  
  stock_n <- which(stock_id == stock_name)
  trib_n1 <- id$tribn[id$stock == stock_name & id$trib == trib_name]
  trib_n2 <- id$tribn2[id$stock == stock_name & id$trib == trib_name]
  tribnodes <- c(paste0("p.S", stock_n, "[39,", trib_n1, "]"), paste0("theta[", trib_n2, ",39]"), paste0("sigma.air[", trib_n2, "]"))
  
  samples <- post_dat$mcmc.info$n.samples
  
  SRparams <-
    data.frame(beta = post_dat$sims.list[["beta"]][, stock_n], 
               lnalpha = post_dat$sims.list[["lnalpha"]][, stock_n],
               S.msy = post_dat$sims.list[["S.msy"]][, stock_n],
               lnalpha.c = post_dat$sims.list[["lnalpha.c"]][, stock_n]) %>%
    as.data.frame() %>%
    dplyr::mutate(R.msy = S.msy * exp(lnalpha.c - beta * S.msy),
                  R.max = 1/beta * exp(lnalpha.c - 1),
                  MSY = R.msy - S.msy) %>%
    dplyr::as.tbl() %>%
    tibble::rownames_to_column(var = "id_var") 
  
  s <- seq(0.1, median(SRparams$S.msy) * 4, by = median(SRparams$S.msy) * 4 / 100)
  c_step <- max(as[[stock_name]][, trib_name], na.rm = TRUE) / 50
  
  tribparams <-
    data.frame(p = post_dat$sims.list[[paste0("p.S", stock_n)]][, 39, trib_n1], 
               theta = post_dat$sims.list[["theta"]][, trib_n2, 39],
               sigma = post_dat$sims.list[["sigma.air"]][, trib_n2]) %>% 
    as.data.frame() %>%
    tibble::rownames_to_column(var = "id_var") %>%
    dplyr::inner_join(data.frame(id_var = as.character(rep(1:samples, each = length(s))), 
                                 s = rep(s, samples), 
                                 stringsAsFactors = FALSE),
                      by = "id_var") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(c = rlnorm(1, log(p * theta * s), sigma)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(maxc = max(c),
                  bin = cut(c, seq(0, max(c), by = c_step), labels = FALSE) * c_step) %>%
    dplyr::arrange(s, bin) %>%
    dplyr::group_by(s, bin) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::mutate(p = n / samples)
  
  profile_s <-
    SRparams %>%
    dplyr::inner_join(data.frame(id_var = as.character(rep(1:samples, each = length(s))), 
                                 s = rep(s, samples), 
                                 stringsAsFactors = FALSE),
                      by = "id_var") %>%
    dplyr::mutate(Rs = s  * exp(lnalpha.c  - beta * s),
                  SY = Rs - s,
                  OYP70 = (SY - 0.7 * MSY) > 0,
                  OYP80 = (SY - 0.8 * MSY) > 0,
                  OYP90 = (SY - 0.9 * MSY) > 0,
                  ORP70 = (Rs - 0.7 * R.max) > 0,
                  ORP80 = (Rs - 0.8 * R.max) > 0,
                  ORP90 = (Rs - 0.9 * R.max) > 0,
                  OFP70 = (SY - 0.7 * MSY) < 0 & (s < S.msy),
                  OFP80 = (SY - 0.8 * MSY) < 0 & (s < S.msy),
                  OFP90 = (SY - 0.9 * MSY) < 0 & (s < S.msy)) %>%
    dplyr::select(s, S.msy, dplyr::starts_with("SY"), dplyr::starts_with("O")) %>%
    dplyr::group_by(s) %>%
    dplyr::summarise_at(dplyr::vars(dplyr::starts_with("O")), mean)
  
  dplyr::inner_join(profile_s, tribparams, by = "s") %>%
    dplyr::mutate_at(.vars = dplyr::vars(dplyr::starts_with("O")), .funs = dplyr::funs(. * p)) %>%
    dplyr::group_by(bin) %>%
    dplyr::summarise_at(.vars = dplyr::vars(dplyr::starts_with("O")), .funs = dplyr::funs(sum(.) / sum(p))) %>%
    dplyr::mutate(stock_name = stock_name, trib_name = trib_name)
}


#' Adds 3-year average to harvest matrix
#'
#' This function creates a matrix containing harvest data for use in jags. If the SRA includes years without statewide harvest 
#' survey data a 3-year average is used for the missing years.
#'
#' @param dat 
#'
#' @return A matrix
#'
#' @examples
#' Ha.hat <- get_Hhat(Ha)
#' Hd.hat <- get_hhat(Hd)
#'
#' @export
get_Hhat <- function(dat){
  stopifnot(exists("trib_id", .GlobalEnv),
            "package:SusitnaEG" %in% search())
  if(dim(dat)[1] < length(year_id)){
    Ha.hat <- 
      as.matrix(dat[, -which(names(Ha) == "year")]) %>% 
      rbind(matrix(apply(dat[(dim(dat)[1] - 2):dim(dat)[1], -1], MARGIN = 2, mean),
                   byrow = TRUE,
                   nrow = length(year_id) - dim(dat)[1], 
                   ncol = dim(dat[, -1])[2])
      )
  } else {Ha.hat <- as.matrix(dat[, -which(names(dat) == "year")])}
  Ha.hat
}


#' Lookup tables for year, age and stock
#'
#' Creates lookup tables to switch between informative names and jags array locations.
#'
#' @param year_range character vector with the indices of abundance included in the analysis.
#' @param age_id named vector of age names and age column position.
#'
#' @return writes 6 objects to R's Global Environment; year_id and stock_id where each object is a named vector where the names are jags array 
#' locations and the elements are informative. trib_id which is a named list with the tributaries comprising each stock. age_id where each object is a named vector where the names are informative
#' and the elements are jags array locations. Also, age_min and age_max the number of years between spawning and recruitment for the youngest and
#' oldest non-negligible age classes.
#'
#' @examples
#' get_ids(1979:2017)
#'
#' @export
get_ids <- function(year_range = 1979:2017,
                    age_id = c("age3" = 1, "age4" = 2, "age5" = 3, "age678" = 4)){
  age_min <- as.numeric(gsub("^age.*(\\d$)", "\\1", names(age_id)[1]))
  age_max <- as.numeric(gsub("^age(\\d).*", "\\1", names(age_id)[length(age_id)]))

  year_id <- as.character(year_range)
  names(year_id) <- 1:length(year_range)
  
  stock_id <- c("Deshka", "East Susitna", "Talkeetna", "Yentna")
  names(stock_id) <- 1:length(stock_id)
  
  trib_id <- 
  list(
    Deshka = c("Deshka"),
    "East Susitna" = c("Goose", "Kashwitna", "Little Willow", "Montana", "Sheep", "Willow", "Other East Susitna"),
    Talkeetna = c("Clear", "Prairie", "Other Talkeetna"),
    Yentna = c("Cache", "Lake", "Peters", "Talachulitna", "Other Yentna")
  )

  list <- list(year_id = year_id,
               stock_id = stock_id,
               trib_id = trib_id,
               age_id = age_id,
               age_min = age_min,
               age_max = age_max)
  list2env(list, .GlobalEnv)
}


#' Initial values for Jags model
#'
#' Function to generate random but constrainded initial values.
#'
#'
#' @return A list of initial values
#'
#' @examples
#' get_inits()
#'
#' @export
get_inits <- function(){
  stopifnot(exists("age_id", .GlobalEnv))
  
  list(
    Dscale.S2 = runif(1, 0.1, 0.5),
    Dscale.S3 = runif(1, 0.1, 0.5),
    Dscale.S4 = runif(1, 0.1, 0.5),
    beta = rlnorm(length(stock_id), log(2e-5), 0.4),
    lnalpha = rlnorm(length(stock_id), log(1.6), 0.4),
    log.resid.0 = rnorm(length(stock_id), 0, 1),
    mean.log.R = rnorm(length(stock_id), 11.3, 0.5),
    phi = runif(length(stock_id), 0.25, 0.75),
    tau.R = runif(1, 1, 25),
    tau.white = runif(length(stock_id), 1, 25),
    z.air = runif(sum(sapply(trib_id, function(x) {length(x[!grepl("Other", x)])})), 0, 0.5),
    g.air = runif(sum(sapply(trib_id, function(x) {length(x[!grepl("Other", x)])})), 0, 0.5),
    tau.weir = runif(1, 1, 25),
    ML1 = c(runif(length(age_id) - 1, -1, 1), NA),
    ML2 = c(runif(length(age_id) - 1, -0.1, 0.1), NA)
  )
}


#' Creates a dataset for plotting OYP, ORP and EY plots
#'
#' This function creates a dataframe that can be used by plot_profile()
#'
#' @param post_dat An mcmc object with nodes lnalpha, beta, S.msy, lnalpha.c from which you want to simulate SR relationships.
#' @param stock_name A character element specifying the stock to plot.
#'
#' @return A data.frame
#'
#' @examples
#' get_ids()
#' get_profile(post, stock_id[1])
#' get_profile(post, "East Susitna")
#' lapply(stock_id, get_profile, post_dat = post)
#'
#' @export
get_profile <- function(post_dat, stock_name){
  stopifnot(exists("stock_id", .GlobalEnv))
  
  stock_n <- which(stock_id == stock_name)
  
  samples <- post_dat$mcmc.info$n.chains * post_dat$mcmc.info$n.samples
  
  temp <-
    data.frame(beta = post_dat$sims.list[["beta"]][, stock_n], 
               lnalpha = post_dat$sims.list[["lnalpha"]][, stock_n],
               S.msy = post_dat$sims.list[["S.msy"]][, stock_n],
               lnalpha.c = post_dat$sims.list[["lnalpha.c"]][, stock_n]) %>%
    as.data.frame() %>%
    dplyr::mutate(R.msy = S.msy * exp(lnalpha.c - beta * S.msy),
                  R.max = 1/beta * exp(lnalpha.c - 1),
                  MSY = R.msy - S.msy) %>%
    dplyr::as.tbl() %>%
    tibble::rownames_to_column(var = "id_var")
  
  s <- seq(0, median(temp$S.msy) * 4, by = median(temp$S.msy) * 4 / 1000)
  
  dplyr::inner_join(temp,
                    data.frame(id_var = as.character(rep(1:samples, each = length(s))), 
                               s = rep(s, samples), stringsAsFactors = FALSE),
                    by = "id_var") %>%
    dplyr::mutate(Rs = s  * exp(lnalpha.c  - beta * s),
                  #Rsr = s * exp(lnalpha.c.recent - beta * s),
                  SY = Rs - s,
                  #SYr = Rsr - s,
                  OYP70 = (SY - 0.7 * MSY) > 0,
                  OYP80 = (SY - 0.8 * MSY) > 0,
                  OYP90 = (SY - 0.9 * MSY) > 0,
                  ORP70 = (Rs - 0.7 * R.max) > 0,
                  ORP80 = (Rs - 0.8 * R.max) > 0,
                  ORP90 = (Rs - 0.9 * R.max) > 0,
                  OFP70 = (SY - 0.7 * MSY) < 0 & (s < S.msy),
                  OFP80 = (SY - 0.8 * MSY) < 0 & (s < S.msy),
                  OFP90 = (SY - 0.9 * MSY) < 0 & (s < S.msy),
                  name = stock_name) %>%
    dplyr::select(name, s, S.msy, dplyr::starts_with("SY"), dplyr::starts_with("O"))
}


#' OYP, ORP and OFP probabilities
#'
#' Function can be used in two ways. Specifying a goal range will return the OYP, OFP and ORP probabilities of each end of the goal range. 
#' Specifying OYP or ORP probabilities will return the escapement sizes associated with those probabilities.
#'
#' @param profile_dat Output of the get_profile function
#' @param goal_range A vector with two element c(lower_bound, upper_bound). Defaults to NULL.
#' @param probs A list containing a 2 element vector. The vector is named after the profile of interest (e.g. "OYP90" of "ORP70") while 
#' the vector contains the desired probability at each bound c(lower_bound, upper_bound). Defaults to NULL.
#' 
#' @return a dataframe with 2 rows.
#'
#' @examples
#' get_profileprobs(profile, goal_range = c(10000, 20000))
#' get_profileprobs(profile, probs = list(OYP80 = c(.8, .3))
#'
#' @export
get_profileprobs <- function(profile_dat, goal_range = NULL, probs = NULL){
  stopifnot(sum(is.null(goal_range), is.null(probs)) == 1,
            grepl("OYP|ORP", names(probs)))
  
  if(!is.null(goal_range)){
    print <- 
      profile_dat %>% 
      dplyr::select(-name, -S.msy, -SY) %>%
      dplyr::group_by(s) %>%
      dplyr::summarise_all(mean, na.rm = TRUE) %>%
      dplyr::mutate(lb =abs(s - goal_range[1]),
                    ub = abs(s - goal_range[2])) %>% 
      dplyr::filter(lb == min(lb) | ub == min(ub)) %>%
      dplyr::select(-lb, -ub) %>%
      dplyr::mutate_at(.vars = dplyr::vars(dplyr::starts_with("O")), function(x){round(x, 3)} * 100)
  }
  if(!is.null(probs)){
    temp <-
      profile_dat %>% 
      dplyr::select(-name, -S.msy, -SY) %>%
      dplyr::group_by(s) %>%
      dplyr::summarise_all(mean, na.rm = TRUE)
    
    cut <- min(which(temp[names(probs)] == max(temp[names(probs)])))
    lr <- 1:cut
    ur <- (cut + 1):dim(temp)[1]
    name <- names(probs)
    
    print <-
      rbind(
        temp[lr, ][abs(temp[lr, name] - probs[[1]][1]) == min(abs(temp[lr, name] - probs[[1]][1])), ],
        temp[ur, ][abs(temp[ur, name] - probs[[1]][2]) == min(abs(temp[ur, name] - probs[[1]][2])), ]
      )
  }
  print
}


#' Identify parameters with questionable convergence.
#'
#' Return parameters from a RJags posterior object with an Rhat that exceed a user specified cutoff.
#'
#' @param post Jags posterior object
#' @param cutoff returns parameters with an Rhat that exceeds the cutoff.
#'
#' @return a list with 2 elements.  The parameters with a Rhat exceeding the cutoff and the Rhat values forming the 90%+ quartiles.
#'
#' @examples
#' get_Rhat(post)
#'
#' @export
get_Rhat <- function(post, cutoff = 1.1){
  list(
    data.frame("Rhat" = post$summary[, "Rhat"][post$summary[, "Rhat"] > cutoff & !is.na(post$summary[, "Rhat"])]),
       "R^ quantiles" = quantile(post$summary[, "Rhat"], probs = seq(0.9, 1, by = .01), na.rm = TRUE))
}

