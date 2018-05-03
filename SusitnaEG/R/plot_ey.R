#' Expected sustained yield plot
#'
#' Expected sustained yield plot with 50 percent confidence ribbon
#'
#' @param profile_dat Output of the profile data function
#' @param rug Show scaled statewide goal ranges. Defaults to TRUE.
#' @param goal_range A vector with two element c(lower_bound, upper_bound). Defaults to NA.
#' @param the upper bound of the plot
#'
#' @return A figure
#'
#' @examples
#' plot_ey(get_profile(post, 125000), get_BEGbounds(3297), c(2835, 6330), 125000)
#'
#' @export
plot_ey <- function(profile_dat, rug = TRUE, goal_range = NA, plot_max = NA){
rug_dat <- SusitnaEG:::get_BEGbounds(median(profile_dat$S.msy))
  
plot <- profile_dat %>%
  dplyr::select(s, dplyr::starts_with("SY")) %>%
  dplyr::group_by(s) %>%
  dplyr::summarise(median.SY = median(SY, na.rm = TRUE),
                    p25.SY = quantile(SY, probs = 0.25, na.rm = TRUE),
                    p75.SY = quantile(SY, probs = 0.75, na.rm = TRUE) #median.SYr = median(SYr, na.rm = TRUE)
                   ) %>%
  tidyr::gather(Productivity, SY, median.SY) %>% #, median.SYr) %>%
  ggplot2::ggplot(ggplot2::aes(x = s, y = SY, color = Productivity)) +
    ggplot2::geom_line() +
    ggplot2::geom_ribbon(ggplot2::aes(x = s, ymin = p25.SY, ymax = p75.SY), inherit.aes = FALSE, alpha = 0.1) +
    ggplot2::scale_x_continuous("Spawners", limits = c(0, plot_max), labels = scales::comma) +
    ggplot2::scale_y_continuous("Expected Yield", limits = c(0, NA), labels = scales::comma) +
    #ggplot2::scale_color_manual(name = "Productivity", labels = c("Historic, 1979-2010 broods", "Recent, 2006-10 broods"), values = c("black", "red")) +
    ggplot2::scale_color_manual(name = "Productivity", labels = "1973-2013 broods", values = "black") +
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

