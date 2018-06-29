#' Expected sustained yield plot
#'
#' Expected sustained yield plot with 50 percent confidence ribbon
#'
#' @param profile_dat Output of the profile data function
#' @param rug Show scaled statewide goal ranges. Defaults to TRUE.
#' @param goal_range A vector with two element c(lower_bound, upper_bound). Defaults to NA.
#'
#' @return A figure
#'
#' @examples
#' get_ids()
#' plot_ey(get_profile(post, stock_id[1]), c(5000, 9000))
#' profiles <- lapply(stock_id, get_profile, post_dat = post)
#' lapply(profiles, plot_ey)
#'
#' @export
plot_ey <- function(profile_dat, rug = TRUE, goal_range = NA){
rug_dat <- SusitnaEG:::get_BEGbounds(median(profile_dat$S.msy))
  
plot_dat <- profile_dat %>%
  dplyr::select(s, dplyr::starts_with("SY")) %>%
  dplyr::group_by(s) %>%
  dplyr::summarise(median.SY = median(SY, na.rm = TRUE),
                    p25.SY = quantile(SY, probs = 0.25, na.rm = TRUE),
                    p75.SY = quantile(SY, probs = 0.75, na.rm = TRUE) #median.SYr = median(SYr, na.rm = TRUE)
                   ) %>%
  tidyr::gather(Productivity, SY, median.SY) #, median.SYr) %>%

ymax <- max(plot_dat$p75.SY) * 1.05
xmax <- plot_dat$s[which(plot_dat$p75.SY < 0)[1]]
if(is.na(xmax)) 
  stop("Error: profile does not extend to escapements with zero yield, use a larger s_ub in get_profile()")

plot <-
  ggplot2::ggplot(plot_dat, ggplot2::aes(x = s, y = SY, color = Productivity)) +
    ggplot2::geom_line() +
    ggplot2::geom_ribbon(ggplot2::aes(x = s, ymin = p25.SY, ymax = p75.SY), inherit.aes = FALSE, alpha = 0.1) +
    ggplot2::scale_x_continuous("Spawners", labels = scales::comma) +
    ggplot2::scale_y_continuous("Expected Yield", labels = scales::comma) +
    ggplot2::coord_cartesian(xlim = c(0, xmax), ylim = c(0, ymax)) +
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

