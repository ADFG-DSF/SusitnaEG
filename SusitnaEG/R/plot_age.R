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
#' x.a <- 
#' age[grepl("Deshka", age$location), ] %>%
#'  dplyr::mutate(x34 = x3 + x4,
#'                x678 = x6 + x78) %>%
#'  dplyr::select(x34, x5, x678) %>%
#'  as.matrix()
#'
#' plot_age(x.a, get_summary(post))
#'
#' @export
plot_age <- function(input_dat, stats_dat){
  stopifnot(exists("year_id", .GlobalEnv),
            exists("age_max", .GlobalEnv))
  yr0 <- as.numeric(min(year_id)) - 1
  yr0_p <- yr0 - age_max
  
Q.obs <- as.data.frame(input_dat / rowSums(input_dat)) %>%
  setNames(paste0("age", 1:ncol(input_dat))) %>%
  tibble::rownames_to_column(var = "year") %>%
  tidyr::gather(age, prop, dplyr::starts_with("age")) %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(prop = cumsum(prop), plot = "Age Composition") %>%
  dplyr::ungroup(year) %>%
  dplyr::mutate(year = yr0 + as.numeric(year))

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
  dplyr::mutate(year = (plot != c("Age-at-Maturity")) * (yr0 + year) + (plot == c("Age-at-Maturity")) * (yr0_p + year)) %>%
  ggplot2::ggplot(ggplot2::aes(x = year, y = prop, alpha = age)) +
    ggplot2::geom_area(position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::facet_grid(plot ~ ., scales = "free", switch = "y") +
    ggplot2::scale_x_continuous(breaks = seq(yr0_p, max(year_id), 3), minor_breaks = NULL) +
    ggplot2::scale_y_continuous(minor_breaks = NULL, labels = scales::comma) +
    ggplot2::geom_point(data = Q.obs, size = 3) +
    ggplot2::scale_alpha_discrete(name = NULL, labels = names(age_id)) +
    ggplot2::labs(y = NULL, x = "Year") +
    ggplot2::theme_bw() +
    ggplot2::theme(strip.background = ggplot2::element_rect(colour="white", fill="white"), strip.placement = "outside")
}
