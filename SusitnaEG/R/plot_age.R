#' Plots of composition and abundance by age
#'
#' Faceted plot of age at maturity, age composition and total run by age.  Observed age composition is also plotted.
#'
#' @param input_dat The input dataset for the SRA model
#' @param stats_dat The output from get_summary() for the SRA model mcmc.list ouput
#'
#' @return A figure
#'
#' @examples
#' plot_age(dat_erinput, get_summary(post_er))
#'
#' @export
plot_age <- function(input_dat, stats_dat){
  x <- input_dat[,substr(colnames(input_dat), 1,1)=="x"]
  n.a <- rowSums(x)  #effective sample size
Q.obs <- dplyr::as.tbl(x/n.a) %>%
  setNames(paste0("age", 1:ncol(x.a))) %>%
  tibble::rownames_to_column(var = "year") %>%
  tidyr::gather(age, prop, dplyr::starts_with("age")) %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(prop = cumsum(prop), plot = "Age Composition") %>%
  dplyr::ungroup(year) %>%
  dplyr::mutate(year = 1978 + as.numeric(year))

P.mn <- get_array(stats_dat, "p") %>%
  tidyr::gather(age, prop, dplyr::starts_with("age")) %>%
  dplyr::mutate(plot = "Age-at-Maturity") %>%
  dplyr::rename(year = byear)
Q.mn <- get_array(stats_dat, "q") %>%
  tidyr::gather(age, prop, dplyr::starts_with("age")) %>%
  dplyr::mutate(plot = "Age Composition") %>%
  dplyr::rename(year = cyear)
N.mn <- get_array(stats_dat, "N.ta") %>%
  tidyr::gather(age, prop, dplyr::starts_with("age")) %>%
  dplyr::mutate(plot = "Total Run") %>%
  dplyr::rename(year = cyear)

dplyr::bind_rows(P.mn, Q.mn, N.mn) %>%
  dplyr::mutate(year = (plot != c("Age-at-Maturity")) * (1978 + year) + (plot == c("Age-at-Maturity")) * (1978 - 6 + year)) %>%
  ggplot2::ggplot(ggplot2::aes(x = year, y = prop, alpha = age)) +
    ggplot2::geom_area(position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::facet_grid(plot ~ ., scales = "free", switch = "y") +
    ggplot2::scale_x_continuous(breaks = seq(1973, 2017, 3), minor_breaks = NULL) +
    ggplot2::scale_y_continuous(minor_breaks = NULL, labels = scales::comma) +
    ggplot2::geom_point(data = Q.obs, size = 3) +
    ggplot2::scale_alpha_discrete(name = NULL, labels = c("Age-34", "Age-5", "Age-678")) +
    ggplot2::labs(y = NULL, x = "Year") +
    ggplot2::theme_bw() +
    ggplot2::theme(strip.background = ggplot2::element_rect(colour="white", fill="white"), strip.placement = "outside")
}
