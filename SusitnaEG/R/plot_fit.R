#' Model fit plots
#'
#' Produces a faceted plot of Escapement and Inriver Run with the appropriately scaled indices of abundance that were used as inputs to the model.
#'
#' @param stats_dat The output from get_summary() for the SRA model mcmc.list output
#' @param stock_name A character element specifying the stock to plot.
#'
#' @return A figure
#'
#' @examples
#' get_ids()
#' plot_fit(get_summary(post), stock_id[1])
#' plot_fit(get_summary(post), "East Susitna")
#' lapply(stock_id, plot_fit, stats_dat = get_summary(post))
#'
#' @export
plot_fit <- function(stats_dat, stock_name){
  stopifnot(exists("year_id", .GlobalEnv),
            exists("age_max", .GlobalEnv),
            exists("stock_id", .GlobalEnv),
            "package:SusitnaEG" %in% search())
  yr0 <- as.numeric(min(year_id)) - 1
  yr0_R <- yr0 - age_max

id0 <- lapply(1:5, function(x) colnames(as[[x]]))
names(id0) <- names(as)
id <- data.frame(stock = factor(rep(names(id0), sapply(id0, length)), levels = stock_id), 
                 tribn = unlist(sapply(id0, function(x) 1:length(x)), use.names = FALSE), 
                 tribn2 = 1:length(unlist(id0)),
                 trib = unlist(id0, use.names = FALSE),
                 stringsAsFactors = FALSE) %>%
      dplyr::arrange(stock, tribn, trib)
  
trib <- function(node){
    temp <- stats_dat %>%
      tibble::rownames_to_column() %>%
      dplyr::filter(grepl(paste0("p.S", node, "\\["), rowname)) %>%
      dplyr::mutate(year = as.numeric(gsub("^.*\\[(\\d+).*$", "\\1", rowname)) + yr0,
                    tribn = as.numeric(gsub("^.*,(\\d)]$", "\\1", rowname)),
                    stock = stock_id[[node]],
                    node = node) %>%
      dplyr::filter(tribn != dim(as[[stock_name]])[2] + 1) %>%
      dplyr::select(year, stock, tribn, ps = Mean)
    if(node == 1) data.frame(year = as.numeric(year_id), stock = stock_id[[node]], tribn = 1, ps = 1) else temp
} 

theta <- 
  stats_dat %>%
    tibble::rownames_to_column() %>%
    dplyr::filter(grepl("^theta\\[", rowname)) %>%
    dplyr::mutate(tribn2 = as.numeric(gsub("theta\\[(\\d+),\\d+\\]$", "\\1", rowname)),
                  year = as.numeric(gsub("theta\\[\\d+,(\\d+)\\]$", "\\1", rowname)) + yr0) %>%
    dplyr::left_join(id, by = "tribn2") %>%
    dplyr::select(year, stock, tribn, theta = Mean)

expand <- 
  lapply(1:5, function(x) trib(x)) %>% 
  do.call(rbind, .) %>%
  dplyr::left_join(theta, by = c("year", "stock", "tribn")) %>% 
  dplyr::mutate(ex_weir = ps,
                ex_as = ex_weir * theta, 
                year = as.character(year)) %>%
  dplyr::left_join(id, by = c("stock", "tribn")) %>%
  dplyr::select(year, stock, trib, ex_weir, ex_as)

weirs <- weir %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "year") %>%
  dplyr::mutate(year = year_id[year]) %>%
  tidyr::gather(trib, count, -year) %>%
  dplyr::left_join(expand, by = c("year", "trib")) %>% 
  dplyr::mutate(value = count / ex_weir,
                name = trib,
                type = "weir",
                name_f = "S") %>%
  dplyr::select(year, name, stock, type, name_f, value)

surveys <- lapply(1:5, function(x) data.frame(stock = factor(unname(stock_id[x]), levels = stock_id),
                                              year = rep(unname(year_id), times = dim(as[[stock_id[x]]])[2]),
                                              trib = rep(colnames(as[[stock_id[x]]]), each = dim(as[[stock_id[x]]])[1]), 
                                              count = as[[stock_id[x]]] %>% as.list() %>% do.call(rbind, .),
                                              stringsAsFactors = FALSE)) %>%
  do.call(rbind, .) %>%
  dplyr::left_join(expand, by = c("year", "stock", "trib")) %>%
  dplyr::mutate(value = count / ex_as,
                name = trib,
                type ="survey",
                name_f = "S") %>%
  dplyr::select(year, stock, name, type, name_f, value)

markrecap <- 
  mr[[1]] %>%
  as.data.frame() %>%
  dplyr::mutate(year = year_id) %>%
  tidyr::gather(stock, value, -year) %>%
  dplyr::mutate(name = "Mark-Recapture",
                type = "Mark-Recapture",
                name_f = "IR") %>%
  dplyr::select(year, stock, name, type, name_f, value)

indicies <- 
  rbind(weirs, surveys, markrecap) %>%
    dplyr::mutate(name_f = factor(name_f,
                                levels = c("S", "IR"),
                                labels = c("Escapement", "Inriver Run")),
                  type = factor(type, levels = c("survey", "weir", "Mark-Recapture")),
                  year = as.numeric(year)) %>%
    dplyr::filter(!is.na(value))

pal <- RColorBrewer::brewer.pal(6, "Paired")
breaks <- 
  id %>% 
  dplyr::select(-tribn2) %>%
  dplyr::mutate(name = factor(trib, levels = c(trib, "Mark-Recapture")),
                color = unlist(lapply(sapply(1:5, function(x) sum(stock == stock_id[x])), function(x) pal[1:x]))) %>%
  dplyr::select(-tribn, - trib) %>%
  dplyr::filter(stock == stock_name) %>%
  rbind(data.frame(name = "Mark-Recapture",
                   stock = stock_name,
                   color = "black",
                   stringsAsFactors = FALSE)) %>%
  dplyr::arrange(stock, name, color)
col <-setNames(breaks$color, breaks$name)
sha <- c("survey" = 17, "weir" = 15, "Mark-Recapture" = 19)

stats_dat %>%
  dplyr::select_(value = as.name("50%"), lcb = as.name("2.5%"), ucb = as.name("97.5%")) %>%
  tibble::rownames_to_column() %>%
  dplyr::filter(grepl("^S\\[|^IR\\[", rowname)) %>%
  dplyr::mutate(name_f = factor(gsub("(^.*)\\[.*", "\\1", rowname),
                                levels = c("S", "IR"),
                                labels = c("Escapement", "Inriver Run")),
                stock = stock_id[gsub(".*,(\\d)\\]", "\\1", rowname)],
                year = as.numeric(year_id[gsub(".*\\[(\\d+),.*", "\\1", rowname)])) %>%
  dplyr::filter(stock == stock_name) %>%
  ggplot2::ggplot(ggplot2::aes(x = year, y = value)) +
    ggplot2::geom_line() +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lcb, ymax = ucb), inherit.aes = TRUE, alpha = 0.3) +
    ggplot2::facet_grid(name_f ~ ., scales = "free_y", switch = "y") +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::geom_jitter(data = indicies[indicies$stock == stock_name, ], ggplot2::aes(color = name, shape = type), size = 3, width = .3) +
    #ggplot2::geom_pointrange(data = indicies2, ggplot2::aes(ymin = lb, ymax = ub, color = "IR.hat", shape = "IR.hat")) +
    ggplot2::scale_color_manual(name ="Index",
                                breaks = breaks$name,
                                values = col) +
    ggplot2::scale_shape_manual(name ="Index",
                                breaks = names(sha),
                                values = sha) +
    ggplot2::scale_x_continuous("Year", breaks = seq(min(year_id), max(year_id), 3), minor_breaks = NULL) +
    ggplot2::scale_y_continuous(minor_breaks = NULL, labels = scales::comma) +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(stock_name) +
    ggplot2::theme(strip.background = ggplot2::element_rect(colour="white", fill="white"), strip.placement = "outside")
}
