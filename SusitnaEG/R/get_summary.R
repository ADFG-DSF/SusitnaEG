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
