#' State Variable Plot
#'
#' Produces a faceted plot of escapement, recruitment, total run, Ricker residuals and harvest rate plotted with 95% confidence envelopes.
#'
#' @param stats_dat The output from get_summary() for the SRA model mcmc.list output
#' @param stock_name A character element specifying the stock to plot.
#' @param S_msr Logical (TRUE) indicating if S_msr shoud be included in the escapement panel.  Defaults to FALSE.
#'
#' @return A figure
#'
#' @examples
#' plot_state(get_summary(post), stock_id[1])
#' plot_state(get_summary(post), "East Susitna")
#' lapply(stock_id, plot_state, stats_dat = get_summary(post))
#'
#' @export
plot_state <- function(stats_dat, stock_name, S_msr = FALSE){
  stopifnot(exists("year_id", .GlobalEnv),
            exists("age_max", .GlobalEnv),
            exists("stock_id", .GlobalEnv))
  yr0 <- as.numeric(min(year_id)) - 1
  yr0_R <- yr0 - age_max
  stock <- unname(which(stock_id == stock_name))
  
msy50 <- stats_dat %>%
  dplyr::select_(median = as.name("50%")) %>%
  tibble::rownames_to_column() %>%
  dplyr::filter(grepl(paste0("msy\\[", stock, "\\]"), rowname)) %>%
  dplyr::mutate(name = factor(stringr::str_sub(rowname, stringr::str_locate(rowname, ".")),
                       levels = c("S", "U"),
                       labels = c("Escapement", "Harvest Rate")))

msr50 <- stats_dat %>%
  dplyr::select_(median = as.name("50%")) %>%
  tibble::rownames_to_column() %>%
  dplyr::filter(grepl(paste0("beta\\[", stock, "\\]|lnalpha\\[", stock, "\\]"), rowname)) %>%
  dplyr::mutate(msr = ifelse(rowname == paste0("beta[", stock, "]"), 1 / median, 1-1/exp(median)),
                name = factor(c("S", "U"),
                              levels = c("S", "U"),
                              labels = c("Escapement", "Harvest Rate")))

plot <-
  stats_dat %>%
  dplyr::select_(median = as.name("50%"), lcb = as.name("2.5%"), ucb = as.name("97.5%")) %>%
  tibble::rownames_to_column() %>%
  dplyr::filter(grepl(paste0("^R\\[\\d+,", stock, 
                             "\\]|S\\[\\d+,", stock, 
                             "\\]|N\\[\\d+,", stock, 
                             "\\]|log.resid.vec\\[\\d+,", stock, 
                             "\\]|mu.Habove\\[\\d+,", stock, "\\]"), rowname)) %>%
  dplyr::mutate(name = factor(stringr::str_sub(rowname, 1, stringr::str_locate(rowname, "\\[")[, 1] - 1),
                       levels = c("S", "N", "R", "mu.Habove", "log.resid.vec"),
                       labels = c("Escapement", "Total Run", "Recruitment", "Harvest Rate", "Ricker Residuals")),
         index = as.numeric(stringr::str_sub(rowname, stringr::str_locate(rowname, "[0-9]+"))),
         year = (name != c("Recruitment")) * (yr0 + index) +
           (name == "Recruitment") * (yr0_R + index)) %>%
  dplyr::filter(year >= yr0) %>%
  ggplot2::ggplot(ggplot2::aes(x = year, y = median)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lcb, ymax = ucb), inherit.aes = TRUE, alpha = 0.3) +
    ggplot2::facet_grid(name ~ ., scales = "free_y", switch = "y") +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::scale_x_continuous("Year", breaks = seq(min(year_id), max(year_id), 3), minor_breaks = NULL)  +
    ggplot2::scale_y_continuous(minor_breaks = NULL, labels = scales::comma)  +
    ggplot2::geom_hline(data = msy50, ggplot2::aes(yintercept = median), color = "red", linetype = 2) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = 0), color = "black", linetype = 1) +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(stock_name) +
    ggplot2::theme(strip.background = ggplot2::element_rect(colour="white", fill="white"), strip.placement = "outside")

if(S_msr == TRUE) {plot <- plot +     ggplot2::geom_hline(data = msr50, ggplot2::aes(yintercept = msr), color = "red", linetype = 5)}

plot
}
