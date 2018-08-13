#' OYP, ORP and/or OFP plots
#'
#' Produces a faceted plot of OYP, ORP or OFP with an overlay of the proposed goal range and a rug showing appropriately scaled upper and lower bounds of other statewide goals.
#'
#' @param profile_dat Output of the get_profile function
#' @param limit Upper bound of spawners for plot. Default (NULL) will use 2.25 times S.msy.
#' @param rug Show scaled statewide goal ranges. Defaults to TRUE.
#' @param goal_range A vector with two element c(lower_bound, upper_bound). Defaults to NA.
#' @param profiles the profiles to plot as a character vector e.g. c("OYP", "OFP").  Defaults to c("OYP", "ORP", "OFP").
#'
#' @return A figure
#'
#' @examples
#' plot_profile(get_profile(post, "Deshka"), rug = FALSE, NA, c("OYP", "OFP"))
#'
#' @export
plot_profile <- function(profile_dat, limit = NULL, rug = TRUE, goal_range = NA, profiles = c("OYP", "ORP", "OFP")){
  temp <-unlist(lapply(profiles, function(x){paste0(x, c("70", "80", "90"))}))
  profile_label <- ggplot2::as_labeller(c('OYP' = "Optimum Yield Profile",
                                          'OFP' = "Overfishing Profile",
                                          'ORP' = "Optimum Recruitment Profile"))
  S.msy50 <- median(profile_dat$S.msy) 
  rug_dat <- SusitnaEG:::get_BEGbounds(S.msy50)
  
  if(is.null(limit)){
    xmax <- S.msy50 * 2.25
  }
  else xmax <- limit
  
  plot <- profile_dat %>%
    dplyr::select_("s", .dots = temp) %>%
    dplyr::group_by(s) %>%
    dplyr::filter(s <= xmax) %>%
    dplyr::summarise_all(mean, na.rm = TRUE) %>%
    tidyr::gather("key", "prob", -s, factor_key = TRUE) %>%
    dplyr::mutate(profile = factor(stringr::str_extract(key, "[A-Z]+"),
                                   levels = c("OYP", "OFP", "ORP")),
                  max_pct = stringr::str_extract(key, "[0-9]+")) %>%
    ggplot2::ggplot(ggplot2::aes(x = s, y = prob, linetype = max_pct)) +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous("Spawners", limits = c(0, xmax), labels = scales::comma) +
    ggplot2::scale_y_continuous("Probability", breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
    ggplot2::scale_linetype_discrete(name = "Percent of Max.")+
    ggplot2::facet_grid(profile ~ ., labeller = profile_label) +
    ggplot2::theme_bw()
  
  if(rug == TRUE) {
    plot2 <- plot +     
      ggplot2::geom_rug(ggplot2::aes(x = lb), data = rug_dat, inherit.aes = FALSE, sides = "b", color = "darkgrey") +
      ggplot2::geom_rug(ggplot2::aes(x = ub), data = rug_dat, inherit.aes = FALSE, sides = "b", color = "black")
  }
  else plot2 <- plot
  
  if(!anyNA(goal_range)) {
    plot2 + ggplot2::geom_rect(ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                               data.frame(xmin = goal_range[1], xmax = goal_range[2], ymin = -Inf, ymax = Inf),
                               inherit.aes = FALSE, fill = "red", alpha = 0.2)
  }
  else plot2
}
