#' Horsetail plot of plausible spawn-recruit relationships
#'
#' Produces a horsetail plot of the median Spawn-Recruit relationship.  Plot also shows 40 plausible Spawn-Recruit relationships in the background and Spawner and Recruit estimates with associated 90% CIs.
#'
#' @param post_dat SRA model mcmc.list output
#' @param stat_dat The output from get_summary() for the SRA model mcmc.list output
#' @param upper The upper bound of the plot
#'
#' @return A figure
#'
#' @examples
#' plot_horse(post, get_summary(post), 35000)
#'
#' @export
plot_horse <- function(post_dat, stats_dat, upper){
  stopifnot(exists("year_id", .GlobalEnv),
            exists("age_max", .GlobalEnv))
  yr0 <- as.numeric(min(year_id)) - 1
  yr0_R <- yr0 - age_max
  
  coeflines <- sapply(c("beta", "lnalpha"), function(x){get_post(post, var = x)}) %>% 
    as.data.frame() %>%
    dplyr::sample_n(40) %>%
    as.matrix() %>%
    plyr::alply(1, function(coef) {ggplot2::stat_function(fun=function(x){x * exp(coef[2] - coef[1] * x)}, colour="grey", alpha = 0.5)})

  param_50 <- stats_dat %>%
    tibble::rownames_to_column() %>%
    dplyr::filter(rowname == "lnalpha" | rowname == "beta") %>%
    dplyr::select_(as.name("50%")) %>%
    unlist()

  temp <- stats_dat %>%
    dplyr::select_(median =as.name("50%"), lb = as.name("5%"), ub = as.name("95%")) %>%
    tibble::rownames_to_column() %>%
    dplyr::filter(grepl("^R\\[|S\\[", rowname)) %>%
    dplyr::mutate(name = stringr::str_sub(rowname, 1, stringr::str_locate(rowname, "\\[")[, 1] - 1),
           index = as.numeric(stringr::str_sub(rowname, stringr::str_locate(rowname, "[0-9]+"))),
           year = (name != "R") * (yr0 + index) + (name == "R") * (yr0_R + index))

  v_dat <-  temp %>% dplyr::filter(name == "R") %>% dplyr::select(vlb = lb, vub = ub, year)
  h_dat <-  temp %>% dplyr::filter(name == "S") %>% dplyr::select(hlb = lb, hub = ub, year)
  text_dat <-  temp %>%
    dplyr::select(median, name, year) %>%
    tidyr::spread(name, median) %>%
    dplyr::filter(!is.na(R) & ! is.na(S)) %>%
    dplyr::inner_join(v_dat, by = "year") %>%
    dplyr::inner_join(h_dat, by = "year")

  ggplot2::ggplot(text_dat, ggplot2::aes(x = S, y = R, label = year, ymin = vlb, ymax = vub, xmin = hlb, xmax = hub)) +
    ggplot2::geom_text() +
    ggplot2::geom_errorbar(linetype = 2) +
    ggplot2::geom_errorbarh(linetype = 2) +
    ggplot2::stat_function(fun=function(x){x * exp(param_50[2] - param_50[1] * x)}, size = 2, linetype = 2) +
    coeflines +
    ggplot2::scale_x_continuous("Spawners", limits = c(0, upper), minor_breaks = NULL, labels = scales::comma) +
    ggplot2::scale_y_continuous("Recruits", minor_breaks = NULL, labels = scales::comma) +
    ggplot2::coord_cartesian(xlim = c(0, upper), ylim = c(0, upper)) +
    ggplot2::geom_abline(slope = 1, size = 1) +
    ggplot2::theme_bw() +
    ggplot2::theme(strip.background = ggplot2::element_rect(colour="white", fill="white"))
}
