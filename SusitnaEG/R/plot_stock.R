#' Plots of composition by stock
#'
#' Faceted plot of stock composition for Susitna Drainage. (Need to add observations)
#'
#' @param input_dat telemetry data matrix
#' @param stats_dat The output from get_summary() for the SRA model mcmc.list ouput
#'
#' @return A figure
#'
#' @examples
#' plot_stock(telemetry, get_summary(post))
#'
#' @export
plot_stock <- function(input_dat, stats_dat){
id <- codes[-1, ] %>% tibble::rowid_to_column("stock")
id$name <- factor(id$name, id$name)
  
# obs <- 
#   input_dat %>%
#     dplyr::mutate_all(function(x) ifelse(x == 0, NA, round(x))) %>%
#     (function(x) {x/rowSums(x)}) %>%
#     tibble::rownames_to_column(var = "year") %>%
#     tidyr::gather(code, p, -year) %>%
#     dplyr::left_join(id[, c("name", "drainage", "code")], by = "code") %>%
#     dplyr::rename(stock = name) %>%
#     dplyr::group_by(year) %>%
#     dplyr::mutate(p = cumsum(p)) %>%
#     dplyr::ungroup(year) %>%
#     dplyr::mutate(year = 1978 + as.numeric(year))

fork <- stats_dat[grepl("^pf.main", rownames(stats_dat)), "Mean"] %>%
  dplyr::rename(main = Mean) %>%
  dplyr::mutate(year = 1979:2017,
                yent = 1 - main)

stock <- function(node){
  stats_dat %>%
  tibble::rownames_to_column() %>%
  dplyr::filter(grepl(paste0(node, "\\["), rowname)) %>%
  dplyr::mutate(year = as.numeric(gsub("^.*\\[(\\d+).*$", "\\1", rowname)) + 1978,
                stock0 = as.numeric(gsub("^.*,(\\d)]$", "\\1", rowname)),
                node = node, 
                stockn = ifelse(node == "pm", stock0, stock0 + 6)) %>%
  dplyr::select(year, stockn, p = Mean)
}

dplyr::left_join(rbind(stock("pm"), stock("py")), fork, by = "year") %>%
        dplyr::mutate(p = ifelse(stockn <= 6, p * main, p * yent),
                      stock = id$name[stockn],
                      drainage = id$drainage[stockn]) %>%
        dplyr::select(-main, -yent) %>%
  ggplot2::ggplot(ggplot2::aes(x = year, y = p, fill = stock)) +
    ggplot2::geom_area() +
    ggplot2::facet_grid(drainage ~ ., scales = "free", switch = "y") +
    ggplot2::scale_x_continuous(breaks = seq(1979, 2017, 3), minor_breaks = NULL) +
    ggplot2::scale_y_continuous(minor_breaks = NULL, labels = scales::percent) +
    ggplot2::scale_color_discrete(breaks = id$name) +
    #ggplot2::geom_point(data = obs, ggplot2::aes(color = stock), size = 3) +
    ggplot2::labs(y = NULL, x = "Year") +
    ggplot2::theme(strip.background = ggplot2::element_rect(colour="white", fill="white"), strip.placement = "outside")

}