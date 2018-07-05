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
  stopifnot(exists("year_id", .GlobalEnv),
            exists("age_max", .GlobalEnv),
            exists("stock_id", .GlobalEnv),
            "package:SusitnaEG" %in% search())
  yr0 <- as.numeric(min(year_id)) - 1
  yr0_R <- yr0 - age_max
  
  id0 <- lapply(1:5, function(x) c(colnames(as[[x]]), paste0("Other ", names(as[x]))))
  names(id0) <- names(as)
  id <- data.frame(stock = factor(rep(names(id0), sapply(id0, length)), levels = stock_id), 
                   tribn = unlist(sapply(id0, function(x) 1:length(x)), use.names = FALSE), 
                   trib0 = unlist(id0, use.names = FALSE),
                   stringsAsFactors = FALSE) %>%
        dplyr::mutate(trib = factor(trib0, levels = trib0)) %>%
        dplyr::select(-trib0) %>%
    dplyr::arrange(stock, tribn)

obs_f <- function(stock){
  input_dat[[stock]] %>%
    (function(x) {x/rowSums(x)}) %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    tidyr::gather(trib0, p0, -rowname) %>%
    dplyr::mutate(trib = factor(ifelse(trib0 == "Other", paste0(trib0, " ", stock), trib0), 
                                levels = id$trib[id$stock == stock]),
                  year = unname(year_id[rowname]),
                  stock = stock) %>%
    dplyr::filter(!is.na(p0)) %>%
    dplyr::arrange(year, dplyr::desc(trib)) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(p = cumsum(p0)) %>%
    dplyr::ungroup() %>%
    dplyr::select(year, stock, trib, Mean = p)
}

obs <- 
  lapply(stock_id[-1], obs_f) %>% do.call(rbind, .) %>%
  dplyr::mutate(stock = factor(stock, levels = stock_id))

est <- stats_dat %>%
  tibble::rownames_to_column() %>%
  dplyr::filter(grepl("p.S", rowname)) %>%
  dplyr::mutate(year = as.numeric(gsub("^.*\\[(\\d+).*$", "\\1", rowname)) + yr0,
                stock = factor(unname(stock_id[gsub("p.S(\\d).*", "\\1", rowname)]), levels = stock_id),
                tribn = as.numeric(gsub("^.*,(\\d)]$", "\\1", rowname))) %>%
  dplyr::left_join(id, by = c("stock", "tribn")) %>%
  dplyr::select(stock, year, trib, Mean) 
  
pal <- RColorBrewer::brewer.pal(7, "Paired")
breaks <- id[!id$stock == "Deshka", ] %>% 
  dplyr::mutate(color = unlist(lapply(sapply(2:5, function(x) sum(stock == stock_id[x]) - 1), function(x) c(pal[1:x], "black"))),
                alpha = 1.25 - (as.numeric(stock) - 1)/length(stock_id[-1])) %>%
  dplyr::select(-tribn) %>%
  dplyr::arrange(stock, trib, color, alpha)
col <-setNames(breaks$color, breaks$trib)
alp <-setNames(breaks$alpha, breaks$trib)

  est %>%
    ggplot2::ggplot(ggplot2::aes(x = as.numeric(year), y = Mean, fill = trib, alpha = trib)) +
    ggplot2::geom_area() +
    ggplot2::facet_grid(stock ~ ., switch = "y") +
    ggplot2::scale_x_continuous(breaks = seq(min(year_id), max(year_id), 3), minor_breaks = NULL) +
    ggplot2::scale_y_continuous(minor_breaks = NULL, labels = scales::percent) +
    ggplot2::scale_fill_manual(breaks = breaks$trib, values = col) +
    ggplot2::scale_alpha_manual(breaks = breaks$trib, values = alp) +
    ggplot2::geom_point(data = obs, ggplot2::aes(fill = trib), size = 3, shape = 21, color = "white") +
    ggplot2::labs(y = NULL, x = "Year") +
    ggplot2::theme(strip.background = ggplot2::element_rect(colour="white", fill="white"), strip.placement = "outside")

}
