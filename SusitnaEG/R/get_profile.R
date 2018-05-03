#' Creates a dataset for plotting OYP, ORP and EY plots
#'
#' This function creates a dataframe that can be used by plot_OYP_ORP()
#'
#' @param data An mcmc object with nodes "lnalpha", "beta", "S.msy", "lnalpha.c", "lnalpha.c.recent" from which you want to simulate SR relationships.
#' @param s_ub the largest value of S for which you want to simulate R.
#'
#' @examples
#' get_profile(post, 250000)
#'
#' @export
get_profile <- function(data, s_ub){

selectnodes <-  c("lnalpha", "beta", "S.msy", "lnalpha.c")#, "lnalpha.c.recent")
s <- seq(0, s_ub, by = s_ub / 1000)
samples <- coda::nchain(data) * coda::niter(data)

sapply(selectnodes, function(x){get_post(data, var = x)}) %>% 
  as.data.frame() %>%
  dplyr::mutate(R.msy = S.msy * exp(lnalpha.c - beta * S.msy),
                R.max = 1/beta * exp(lnalpha.c - 1),
                MSY = R.msy - S.msy) %>%
  dplyr::as.tbl() %>%
  tibble::rownames_to_column(var = "id_var") %>%
  dplyr::inner_join(data.frame(id_var = as.character(rep(1:samples, each = length(s))), s = rep(s, samples), stringsAsFactors = FALSE), by = "id_var") %>%
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
