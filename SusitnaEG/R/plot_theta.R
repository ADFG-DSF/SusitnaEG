#' Plots of Aerial Survey detectability
#'
#' Estimated and observed aerial survey detectability in the Susitna River drainage.
#'
#' @param stats_dat The output from get_summary() for the SRA model mcmc.list output
#'
#' @return A figure
#'
#' @examples
#' plot_theta(get_summary(post))
#'
#' @export
plot_theta <- function(stats_dat){
stopifnot("package:SusitnaEG" %in% search())
  
id <- codes[-1, ] %>% tibble::rowid_to_column("stock")
id$name <- factor(id$name, id$name)

theta_est <- stats_dat %>%
  tibble::rownames_to_column() %>%
  dplyr::filter(grepl("^theta\\[", rowname)) %>%
  dplyr::mutate(stockn = as.numeric(gsub("theta\\[(\\d+)\\]$", "\\1", rowname)),
                stock = id$name[stockn]) %>%
  dplyr::select(stock, theta = Mean)

theta_obs1 <- data.frame(stock = id$name[2], theta = as_complete$C / weir$count[weir$group == "C"])
theta_obs2 <- data.frame(stock = id$name[3], theta = c(1304, 953) / weir$count[weir$group == "E"])
theta_obs <- rbind(theta_obs1, theta_obs2)

ggplot2::ggplot(theta_est, ggplot2::aes(x = stock, y = theta)) +
    ggplot2::geom_col() +
    ggplot2::geom_dotplot(data = theta_obs, binaxis = "y", stackdir = "center")
}
