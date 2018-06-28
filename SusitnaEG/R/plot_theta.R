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
stopifnot(exists("stock_id", .GlobalEnv),
          "package:SusitnaEG" %in% search())
  
id0 <- lapply(1:5, function(x) colnames(as[[x]]))
names(id0) <- names(as)
id <- data.frame(stock = factor(rep(names(id0), sapply(id0, length)), levels = stock_id), 
                 tribn = unlist(sapply(id0, function(x) 1:length(x)), use.names = FALSE), 
                 trib = unlist(id0, use.names = FALSE))
  
theta_est <- stats_dat %>%
  tibble::rownames_to_column() %>%
  dplyr::filter(grepl("^theta.S.*", rowname)) %>%
  dplyr::mutate(rowname = ifelse(rowname == "theta.S1", "theta.S1[1]", rowname),
                tribn = as.numeric(gsub("^.*\\[(\\d)\\]", "\\1", rowname)),
                stockn = as.numeric(gsub("^.*.S(\\d)\\[.*", "\\1", rowname)),
                stock = factor(unname(stock_id[stockn]), levels = stock_id)) %>%
  dplyr::select(stock, tribn, theta = Mean) %>%
  dplyr::left_join(id, by = c("stock", "tribn"))

theta_obs1 <- data.frame(stock = stock_id[1], trib = "Deshka", theta = as_complete$C / weir$count[weir$group == "C"])
theta_obs2 <- data.frame(stock = stock_id[2], trib = "Montana", theta = c(1304, 953) / weir$count[weir$group == "E"])
theta_obs <- rbind(theta_obs1, theta_obs2)

ggplot2::ggplot(theta_est, ggplot2::aes(x = trib, y = theta)) +
    ggplot2::geom_col() +
    ggplot2::geom_dotplot(data = theta_obs, binaxis = "y", stackdir = "center") +
    ggplot2::facet_grid(. ~ stock, scales = "free_x") +
    theme(axis.text.x=element_text(angle = 45, hjust = 1, vjust = 1))
}
