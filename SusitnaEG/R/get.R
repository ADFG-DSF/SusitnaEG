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
            "package:SusitnaEG" %in% search())
  
  id0 <- lapply(1:5, function(x) colnames(as[[x]]))
  names(id0) <- names(as)
  id <- data.frame(stock = factor(rep(names(id0), sapply(id0, length)), levels = stock_id), 
                   tribn = unlist(sapply(id0, function(x) 1:length(x)), use.names = FALSE), 
                   tribn2 = 1:length(unlist(id0)),
                   trib = unlist(id0, use.names = FALSE),
                   stringsAsFactors = FALSE) %>%
    dplyr::arrange(stock, tribn, trib)
  
  stock_n <- which(stock_id == stock_name)
  trib_n1 <- id$tribn[id$stock == stock_name & id$trib == trib_name]
  trib_n2 <- id$tribn2[id$stock == stock_name & id$trib == trib_name]
  tribnodes <- c(paste0("p.S", stock_n, "[39,", trib_n1, "]"), paste0("theta[", trib_n2, ",39]"), paste0("sigma.air[", trib_n2, "]"))
  SRnodes <-  paste0(c("lnalpha[", "beta[", "S.msy[", "lnalpha.c["), stock_n, "]")
  
  samples <- coda::nchain(post_dat) * coda::niter(post_dat)
  
  SRparams <-
    sapply(SRnodes, function(x){get_post(post_dat, var = x)}) %>% 
    as.data.frame() %>%
    setNames(gsub("(^.*)\\[\\d\\]", "\\1", SRnodes)) %>%
    dplyr::mutate(R.msy = S.msy * exp(lnalpha.c - beta * S.msy),
                  R.max = 1/beta * exp(lnalpha.c - 1),
                  MSY = R.msy - S.msy) %>%
    dplyr::as.tbl() %>%
    tibble::rownames_to_column(var = "id_var") 
  
  s <- seq(0.1, median(SRparams$S.msy) * 4, by = median(SRparams$S.msy) * 4 / 100)
  c_step <- max(as[[stock_name]][, trib_name], na.rm = TRUE) / 50
  
  tribparams <-
    sapply(tribnodes, function(x){get_post(post_dat, var = x)}) %>% 
    as.data.frame() %>%
    setNames(c("p", "theta", "sigma")) %>%
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
  
  stock_id <- c("Deshka", "East Susitna", "Talkeetna", "Yentna", "Other")
  names(stock_id) <- 1:length(stock_id)
  
  trib_id <- 
  list(
    Deshka = c("Deshka"),
    "East Susitna" = c("Goose", "Kashwitna", "Little Willow", "Montana", "Sheep", "Willow", "Other East Susitna"),
    Talkeetna = c("Clear", "Prairie", "Other Talkeetna"),
    Yentna = c("Cache", "Lake", "Peters", "Talachulitna", "Other Yentna"),
    Other = c("Chulitna", "Indian", "Portage", "Other Other")
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
    Dscale.S5 = runif(1, 0.1, 0.5),
    beta = rlnorm(5, log(2e-5), 0.4),
    lnalpha = rlnorm(5, log(1.6), 0.4),
    log.resid.0 = rnorm(5, 0, 1),
    mean.log.R = rnorm(5, 11.3, 0.5),
    phi = runif(5, 0.25, 0.75),
    tau.R = runif(1, 1, 25),
    tau.white = runif(5, 1, 25),
    z.air = runif(16, 0, 0.5),
    g.air = runif(16, 0, 0.5),
    tau.weir = runif(1, 1, 25),
    ML1 = c(runif(length(age_id) - 1, -1, 1), NA),
    ML2 = c(runif(length(age_id) - 1, -0.1, 0.1), NA)
  )
}


#' Get Posterior Summaries
#'
#' @description This function allows you to pull out variables from a mcmc.list or matrix.
#'    Modifided version of oriiginal written by Ben Stanton (bas0041@auburn.edu).
#'
#' @param post_dat an object of class 'mcmc.list' or 'matrix'
#' @param var the variable you wish to view. Must be in "quotes". To pull out a vector specify thru the opening bracket e.g. "N["
#'
#' @examples get.post(post_er, var="N[")
#' @examples get.post(post_er, var="lnalpha")
#'
#' @export
get_post=function(post_dat, var){
  #coerce to matrix if mcmc.list
  if (!coda::is.mcmc.list(post_dat) & !is.matrix(post_dat)) stop("post_dat is not of class mcmc.list or matrix")
  if (coda::is.mcmc.list(post_dat)) post_dat=as.matrix(post_dat)
  
  #pull out posteriors for requested variable
  if(substr(var,nchar(var), nchar(var))=="[") post=post_dat[,substr(colnames(post_dat), 1, nchar(var))==var] else post=post_dat[,var]
  post
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
  selectnodes <-  paste0(c("lnalpha[", "beta[", "S.msy[", "lnalpha.c["), stock_n, "]")
  
  samples <- coda::nchain(post_dat) * coda::niter(post_dat)
  
  temp <-
    sapply(selectnodes, function(x){get_post(post_dat, var = x)}) %>% 
    as.data.frame() %>%
    setNames(gsub("(^.*)\\[\\d\\]", "\\1", selectnodes)) %>%
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
                  OFP90 = (SY - 0.9 * MSY) < 0 & (s < S.msy)) %>%
    dplyr::select(s, S.msy, dplyr::starts_with("SY"), dplyr::starts_with("O"))
}


#' Posterior summaries
#'
#' Posterior summaries in a format expected by KenaiSRA plotting functions
#'
#' @param post An mcmc.list to be summarised
#'
#' @return Data frame with means, sd and quantiles of posterior estimates.
#'
#' @examples
#' get_summary(post_er)
#'
#' @export
get_summary <- function(post){
  sumout <- coda:::summary.mcmc.list(post, quantiles = c(0.025, 0.05, 0.25, 0.5, 0.75, 0.95, 0.975))
  statsquants <- as.data.frame(cbind(sumout$statistics,sumout$quantiles))
  #write.csv(statsquants, file= paste("BFG",version,"statsquants.csv") )
  dplyr::as.tbl(statsquants)
}

