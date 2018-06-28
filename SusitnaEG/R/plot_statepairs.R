#' Ricker Posterior correlations plot
#'
#' Pairs plot for beta, ln(alpha), phi, S.msy and sigma.white
#'
#' @param dat_post posterior object
#' @param pars nodes to plot. Defaults to beta, ln(alpha), phi, S.msy and sigma.white.
#' @param n number of pairs to plot. Defaults to 200.
#' @param trim percentage of extreme S.msy realizations to remove. Defaults to 0.05.
#'
#' @return A figure
#'
#' @examples
#' plot_statepairs(post)
#'
#' @export
plot_statepairs <- function(dat_post, 
                            pars = c(paste0("beta[", 1:5, "]"),
                                     paste0("lnalpha[", 1:5, "]"),
                                     'phi',
                                     paste0('S.msy[', 1:5, "]"),
                                     'sigma.white'), 
                            n = 200, 
                            trim = 0.05){
  postdf <- as.data.frame(as.matrix(dat_post))   
  bounds <- lapply(postdf[, grepl("S.msy", names(postdf))], quantile, probs = c(trim / 2, 1 - trim / 2))
  index <- Reduce(intersect, lapply(1:5, function(x) which(postdf[names(temp)][, x] > temp[[x]][1] & postdf[names(temp)][, x] < temp[[x]][2])))
  subset <- postdf[index, pars]
  pairs(subset[sample(1:dim(subset)[1], n), ], cex=0.6)
}
