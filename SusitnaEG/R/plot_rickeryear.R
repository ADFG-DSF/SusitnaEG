#' Ricker Curves with annual productivity
#'
#' logRhat.y <- log(S.y) + lnalpha - beta * S.y 
#' logresid.y <- log(R.y) - logRhat.y
#' lnalpha.y <- lnalpha + logresid.y
#'
#' @param stats_dat The output from get_summary() for the SRA model mcmc.list output
#'
#' @return A figure
#'
#' @examples
#' plot_rickeryear(get_summary(post_er))
#'
#' @export
plot_rickeryear <- function(stats_dat){
  stopifnot(exists("year_id", .GlobalEnv),
            exists("age_max", .GlobalEnv))
  yr0 <- as.numeric(min(year_id)) - 1
  yr0_R <- yr0 - age_max
  
text <- stats_dat %>%
  dplyr::select_(median =as.name("50%")) %>%
  tibble::rownames_to_column() %>%
  dplyr::filter(grepl("^R\\[|S\\[", rowname)) %>%
  dplyr::mutate(name = stringr::str_sub(rowname, 1, stringr::str_locate(rowname, "\\[")[, 1] - 1),
                index = as.numeric(stringr::str_sub(rowname, stringr::str_locate(rowname, "[0-9]+"))),
                year = (name != "R") * (yr0 + index) + (name == "R") * (yr0_R + index)) %>%
  dplyr::select(median, name, year) %>%
  tidyr::spread(name, median) %>%
  dplyr::filter(!is.na(R) & ! is.na(S))

lines <- stats_dat %>%
  dplyr::select(value = Mean) %>%
  tibble::rownames_to_column()  %>% 
  dplyr::filter(grepl("^lnalpha.vec", rowname)) %>%
  dplyr::mutate(#index = as.numeric(gsub("^.*\\[(\\d+)\\]", "\\1", rowname)),
                #year = as.character(yr0 + index),
                beta = as.numeric(stats_dat[grepl("beta", rownames(stats_dat)), "Mean"])) %>%
  dplyr::select(value, beta) %>%
  as.matrix() %>%
  plyr::alply(1, function(coef) {ggplot2::stat_function(fun = function(x){x * exp(coef[1] - coef[2] * x)}, colour="grey", alpha = 0.5)})

  
  ggplot2::ggplot(text, ggplot2::aes(x = S, y = R, label = year)) +
    ggplot2::geom_text() + 
    lines +
    ggplot2::scale_x_continuous("Spawners", limits = c(0, NA), minor_breaks = NULL, labels = scales::comma) +
    ggplot2::scale_y_continuous("Recruits", limits = c(0, NA), minor_breaks = NULL, labels = scales::comma) +
    ggplot2::theme_bw() +
    ggplot2::theme(strip.background = ggplot2::element_rect(colour="white", fill="white"))
}
