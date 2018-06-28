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
plot_rickeryear <- function(stats_dat, stock_name){
  stopifnot(exists("year_id", .GlobalEnv),
            exists("stock_id", .GlobalEnv),
            exists("age_max", .GlobalEnv))
  yr0 <- as.numeric(min(year_id)) - 1
  yr0_R <- yr0 - age_max
  
stock_n <- which(stock_id == stock_name)
  
text <- stats_dat %>%
  dplyr::select_(median =as.name("50%")) %>%
  tibble::rownames_to_column() %>%
  dplyr::filter(grepl(paste0("^R\\[\\d+,", stock_n, "\\]|S\\[\\d+,", stock_n, "\\]"), rowname)) %>%
  dplyr::mutate(name = stringr::str_sub(rowname, 1, stringr::str_locate(rowname, "\\[")[, 1] - 1),
                index = as.numeric(stringr::str_sub(rowname, stringr::str_locate(rowname, "[0-9]+"))),
                year = (name != "R") * (yr0 + index) + (name == "R") * (yr0_R + index)) %>%
  dplyr::select(median, name, year) %>%
  tidyr::spread(name, median) %>%
  dplyr::filter(!is.na(R) & ! is.na(S))

lines <- stats_dat %>%
  dplyr::select(value = Mean) %>%
  tibble::rownames_to_column()  %>% 
  dplyr::filter(grepl(paste0("^lnalpha.vec\\[\\d+,", stock_n, "\\]"), rowname)) %>%
  dplyr::mutate(beta = as.numeric(stats_dat[grepl(paste0("beta\\[", stock_n, "\\]"), rownames(stats_dat)), "Mean"])) %>%
  dplyr::select(value, beta) %>%
  as.matrix() %>%
  plyr::alply(1, function(coef) {ggplot2::stat_function(fun = function(x){x * exp(coef[1] - coef[2] * x)}, colour="grey", alpha = 0.5)})

upper = max(text[, c("R", "S")]) * 1.1
  
  ggplot2::ggplot(text, ggplot2::aes(x = S, y = R, label = year)) +
    ggplot2::geom_text() +
    lines +
    ggplot2::scale_x_continuous("Spawners", limits = c(0, upper), minor_breaks = NULL, labels = scales::comma) +
    ggplot2::scale_y_continuous("Recruits", limits = c(0, NA), minor_breaks = NULL, labels = scales::comma) +
    ggplot2::coord_cartesian(xlim = c(0, upper), ylim = c(0, upper)) +
    ggplot2::theme_bw() +
    ggplot2::theme(strip.background = ggplot2::element_rect(colour="white", fill="white"))
}
