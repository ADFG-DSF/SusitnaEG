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
stopifnot(exists("year_id", .GlobalEnv),
          exists("stock_id", .GlobalEnv),
          "package:SusitnaEG" %in% search())
  
yr0 <- as.numeric(min(year_id)) - 1
  
id0 <- lapply(1:5, function(x) colnames(as[[x]]))
names(id0) <- names(as)
id <- data.frame(stock = factor(rep(names(id0), sapply(id0, length)), levels = stock_id), 
                 tribn = unlist(sapply(id0, function(x) 1:length(x)), use.names = FALSE), 
                 tribn2 = 1:length(unlist(id0)),
                 trib = unlist(id0, use.names = FALSE))

theta_est <- 
  tibble::rownames_to_column(stats_dat) %>% 
  dplyr::filter(grepl(paste0("^theta\\["), rowname)) %>% 
  dplyr::mutate(year = as.numeric(gsub("theta\\[\\d+,(\\d+)\\]", "\\1", rowname)) + yr0,
                tribn2 = as.numeric(gsub("theta\\[(\\d+),\\d+\\]", "\\1", rowname))) %>%
  dplyr::left_join(id, by = "tribn2") %>%
  dplyr::select(year, stock, trib, theta = Mean)

theta_obs1 <- data.frame(year = as.numeric(year_id), stock = unname(stock_id[1]), trib = "Deshka", theta = as[["Deshka"]][, "Deshka"] / weir[, "Deshka"])
theta_obs2 <- data.frame(year = as.numeric(year_id), stock = unname(stock_id[2]), trib = "Montana", theta = as[["East Susitna"]][, "Montana"] / weir[, "Montana"])
theta_obs3 <- data.frame(year = as.numeric(year_id), stock = unname(stock_id[2]), trib = "Willow", theta = as[["East Susitna"]][, "Willow"] / weir[, "Willow"])
theta_obs <- 
  rbind(theta_obs1, theta_obs2, theta_obs3) %>%
  dplyr::filter(!is.na(theta))

pal <- RColorBrewer::brewer.pal(6, "Paired")
breaks <- 
  id %>% 
  dplyr::select(-tribn, -tribn2) %>%
  dplyr::mutate(color = unlist(lapply(sapply(1:5, function(x) sum(stock == stock_id[x])), function(x) pal[1:x]))) %>%
  dplyr::arrange(stock, trib, color)
col <-setNames(breaks$color, breaks$trib)

ggplot2::ggplot(theta_est, ggplot2::aes(x = year, y = theta, color = trib)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(data = theta_obs, size = 3) +
    ggplot2::facet_grid(. ~ stock) +
    ggplot2::scale_color_manual(name ="Trib",
                                breaks = breaks$trib,
                                values = col)
}
