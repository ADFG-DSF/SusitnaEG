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
