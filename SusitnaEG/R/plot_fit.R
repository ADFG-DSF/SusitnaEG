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
                 trib = unlist(id0, use.names = FALSE),
                 stringsAsFactors = FALSE) %>%
      dplyr::arrange(stock, tribn, trib)
  
trib <- function(node){
    temp <- stats_dat %>%
      tibble::rownames_to_column() %>%
      dplyr::filter(grepl(paste0("p.S", node, "\\["), rowname)) %>%
      dplyr::mutate(year = as.numeric(gsub("^.*\\[(\\d+).*$", "\\1", rowname)) + yr0,
                    tribn = as.numeric(gsub("^.*,(\\d)]$", "\\1", rowname)),
                    node = node) %>%
      dplyr::filter(tribn != dim(as[[stock_name]])[2] + 1) %>%
      dplyr::select(year, tribn, ps = Mean)
    if(node == 1) data.frame(year = as.numeric(year_id), tribn = 1, ps = 1) else temp
} 

theta <- function(node){
  stats_dat %>%
    tibble::rownames_to_column() %>%
    dplyr::mutate(rowname = ifelse(rowname == "theta.S1", "theta.S1[1]", rowname)) %>%
    dplyr::filter(grepl(paste0("theta.S", node, "\\["), rowname)) %>%
    dplyr::mutate(tribn = as.numeric(gsub("^.*\\[(\\d)\\]$", "\\1", rowname))) %>%
    dplyr::select(tribn, theta = Mean)
}

expand <- 
  lapply(1:5, function(x) trib(x) %>% 
                          dplyr::left_join(theta(x), by = "tribn") %>% 
                          dplyr::mutate(stock = factor(unname(stock_id[x]), levels = stock_id))) %>% 
  do.call(rbind, .) %>%
  dplyr::mutate(ex_weir = ps,
                ex_as = ex_weir * theta, 
                year = as.character(year)) %>%
  dplyr::left_join(id, by = c("stock", "tribn")) %>%
  dplyr::select(year, stock, trib, ex_weir, ex_as)

weirs <- weir[, c("year", "trib", "count")] %>%
  dplyr::mutate(trib = gsub(" Weir| weir", "", trib)) %>%
  dplyr::left_join(expand, by = c("year", "trib")) %>% 
  dplyr::mutate(value = count / ex_weir,
                name = paste0(trib, " weir"),
                name_f = "S") %>%
  dplyr::select(year, name, stock,  name_f, value)

surveys <- lapply(1:5, function(x) data.frame(stock = factor(unname(stock_id[x]), levels = stock_id),
                                              year = rep(unname(year_id), times = dim(as[[stock_id[x]]])[2]),
                                              trib = rep(colnames(as[[stock_id[x]]]), each = dim(as[[stock_id[x]]])[1]), 
                                              count = as[[stock_id[x]]] %>% as.list() %>% do.call(rbind, .),
                                              stringsAsFactors = FALSE)) %>%
  do.call(rbind, .) %>%
  dplyr::left_join(expand, by = c("year", "stock", "trib")) %>%
  dplyr::mutate(value = count / ex_as,
                name = paste0(trib, " survey"),
                name_f = "S") %>%
  dplyr::select(year, stock, name, name_f, value)

draw <- MCMCpack::rdirichlet(dim(mr)[[1]], c(15,15,10,5))
markrecap <- 
  data.frame(mr$mr_mainstem*draw[,1], 
                mr$mr_mainstem*draw[,2], 
                mr$mr_mainstem*draw[,3], 
                mr$mr_yentna, 
                mr$mr_mainstem*draw[,4]) %>%
  setNames(stock_id) %>%
  dplyr::mutate(year = year_id) %>%
  tidyr::gather(stock, value, -year) %>%
  dplyr::mutate(name = "Mark-Recapture",
                name_f = "IR") %>%
  dplyr::select(year, stock, name, name_f, value)

indicies <- 
  rbind(weirs, surveys, markrecap) %>%
    dplyr::mutate(name_f = factor(name_f,
                                levels = c("S", "IR"),
                                labels = c("Escapement", "Inriver Run")),
                  year = as.numeric(year)) %>%
    dplyr::filter(!is.na(value))

pal <- RColorBrewer::brewer.pal(7, "Paired")
breaks <- id %>% 
  dplyr::mutate(name = paste0(trib, " survey"),
                color = unlist(lapply(sapply(1:5, function(x) sum(stock == stock_id[x])), function(x) pal[1:x])),
                shape = 17) %>%
  dplyr::select(-tribn, - trib) %>%
  dplyr::arrange(stock, name, color, shape) %>%
  rbind(data.frame(name = c("Deshka weir", "Montana weir", rep("Mark-Recapture", 5)),
                   stock = c("Deshka", "East Susitna", stock_id),
                   color = rep("black", 7),
                   shape = c(18, 18, rep(19, 5)))) %>%
  dplyr::arrange(stock, name)

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
    ggplot2::geom_jitter(data = indicies[indicies$stock == stock_name, ], ggplot2::aes(color = name, shape = name), size = 3, width = .3) +
    #ggplot2::geom_pointrange(data = indicies2, ggplot2::aes(ymin = lb, ymax = ub, color = "IR.hat", shape = "IR.hat")) +
    ggplot2::scale_color_manual(name ="Index",
                                breaks = breaks$name[breaks$stock == stock_name],
                                values = breaks$color[breaks$stock == stock_name]) +
    ggplot2::scale_shape_manual(name ="Index",
                                breaks = breaks$name[breaks$stock == stock_name],
                                values = breaks$shape[breaks$stock == stock_name]) +
    ggplot2::scale_x_continuous("Year", breaks = seq(min(year_id), max(year_id), 3), minor_breaks = NULL) +
    ggplot2::scale_y_continuous(minor_breaks = NULL, labels = scales::comma) +
    ggplot2::theme_bw() +
    ggplot2::theme(strip.background = ggplot2::element_rect(colour="white", fill="white"), strip.placement = "outside")
}
