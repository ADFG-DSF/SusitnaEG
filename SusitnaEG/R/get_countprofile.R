#' Creates a dataset for plotting OYP, ORP and EY plots based on aerial count data
#'
#' This function creates a dataframe that can be used by plot_countprofile()
#'
#' @param data An mcmc object with nodes lnalpha, beta, S.msy, lnalpha.c, p.S#, theta, and sigma.air from which you want 
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
