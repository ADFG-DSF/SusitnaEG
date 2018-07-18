#' OYP, ORP and/or OFP plots based on aerial counts
#'
#' Produces a faceted plot of OYP, ORP or OFP with an overlay of the proposed goal range.
#'
#' @param profile_dat Output of the get_profile function
#' @param limit Upper bound of spawners for plot. Default (NULL) will use the maximum observed count.
#' @param goal_range A vector with two element c(lower_bound, upper_bound). Defaults to NA.
#' @param profiles the profiles to plot as a character vector e.g. c("OYP", "OFP").  Defaults to c("OYP", "ORP", "OFP").
#'
#' @return A figure
#'
#' @examples
#' plot_countprofile(get_countprofile(post, 200000), c("OYP", "OFP"))
#'
#' @export
plot_countprofile <- function(profile_dat, limit = NULL, goal_range = NA, profiles = c("OYP", "ORP", "OFP")){
  stopifnot(exists("stock_id", .GlobalEnv),
            "package:SusitnaEG" %in% search())
  
  temp <-unlist(lapply(profiles, function(x){paste0(x, c("70", "80", "90"))}))
  profile_label <- ggplot2::as_labeller(c('OYP' = "Optimum Yield Profile",
                                          'OFP' = "Overfishing Profile",
                                          'ORP' = "Optimum Recruitment Profile"))

  if(is.null(limit)){
    xmax <- max(as[[unique(profile_dat$stock_name)]][, unique(profile_dat$trib_name)], na.rm = TRUE)
  }
  else xmax <- limit
  
  plot <- 
    profile_dat %>%
    dplyr::select_("bin", .dots = temp) %>%
    dplyr::filter(bin <= xmax) %>%
    tidyr::gather("key", "prob", -bin, factor_key = TRUE) %>%
    dplyr::mutate(profile = factor(stringr::str_extract(key, "[A-Z]+"),
                                   levels = c("OYP", "OFP", "ORP")),
                  max_pct = stringr::str_extract(key, "[0-9]+")) %>%
    ggplot2::ggplot(ggplot2::aes(x = bin, y = prob, linetype = max_pct)) +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous("Count", limits = c(0, xmax), labels = scales::comma) +
    ggplot2::scale_y_continuous("Probability", breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
    ggplot2::scale_linetype_discrete(name = "Percent of Max.")+
    ggplot2::facet_grid(profile ~ ., labeller = profile_label) +
    ggplot2::ggtitle(paste0(unique(profile_dat$stock_name), ": ", unique(profile_dat$trib_name))) +
    ggplot2::theme_bw()
  
  if(!anyNA(goal_range)) {
    plot + ggplot2::geom_rect(ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                               data.frame(xmin = goal_range[1], xmax = min(xmax, goal_range[2]), ymin = -Inf, ymax = Inf),
                               inherit.aes = FALSE, fill = "red", alpha = 0.2)
  }
  else plot
}
