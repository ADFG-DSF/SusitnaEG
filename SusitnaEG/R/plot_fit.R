#' Model fit plots
#'
#' Produces a faceted plot of Escapement and inriver run with the appropriately scaled indices of abundance that were used as inputs to the model.
#'
#' @param stats_dat The output from get_summary() for the SRA model mcmc.list output
#'
#' @return A figure
#'
#' @examples
#' plot_fit(get_summary(post))
#'
#' @export
plot_fit <- function(stats_dat){
  stopifnot(exists("year_id", .GlobalEnv),
            exists("age_max", .GlobalEnv),
            "package:SusitnaEG" %in% search())
  yr0 <- as.numeric(min(year_id)) - 1
  yr0_R <- yr0 - age_max
  
id <- codes[-1, ] %>% tibble::rowid_to_column("stockn")
  
fork <- stats_dat%>%
  tibble::rownames_to_column() %>%
  dplyr::filter(grepl("^pf.main.*", rowname)) %>%
  dplyr::mutate(param = gsub("(^.*)\\[.*", "\\1", rowname),
                year = as.numeric(gsub("^pf.main\\[(\\d+)\\]", "\\1", rowname)) + yr0) %>%
  dplyr::select(year, pf = Mean)
  
stock <- function(node){
    stats_dat %>%
      tibble::rownames_to_column() %>%
      dplyr::filter(grepl(paste0(node, "\\["), rowname)) %>%
      dplyr::mutate(year = as.numeric(gsub("^.*\\[(\\d+).*$", "\\1", rowname)) + yr0,
                    stock0 = as.numeric(gsub("^.*,(\\d)]$", "\\1", rowname)),
                    node = node, 
                    stockn = ifelse(node == "pm", stock0, stock0 + 6)) %>%
      dplyr::select(year, stockn, ps = Mean)
} 

theta <- stats_dat%>%
  tibble::rownames_to_column() %>%
  dplyr::filter(grepl("^theta.*", rowname)) %>%
  dplyr::mutate(param = gsub("(^.*)\\[.*", "\\1", rowname),
                stockn = as.numeric(gsub("^.*\\[(\\d+)\\]", "\\1", rowname))) %>%
  dplyr::select(stockn, theta = Mean)

expand <- 
  dplyr::left_join(rbind(stock("pm"), stock("py")), fork, by = "year") %>%
    dplyr::left_join(theta, by = "stockn") %>%
    dplyr::mutate(ex_weir = ifelse(stockn <= 6, pf * ps, (1 - pf) * ps),
                  ex_as = ex_weir * theta, 
                  year = as.character(year))

weirs <- weir[weir$trib == "Deshka Weir", c("year", "trib", "count")] %>%
  dplyr::mutate(stockn = id$stockn[grepl("^Deshka.*", id$name)]) %>%
  dplyr::left_join(expand, by = c("year", "stockn")) %>% 
  dplyr::mutate(value = count / ex_weir,
                name_f = "Smain") %>%
  dplyr::select(year, name = trib, name_f, value)

surveys <- as_complete[, -1] %>% 
  tidyr::gather(code, count, - year) %>%
  dplyr::left_join(id, by = "code") %>%
  dplyr::left_join(expand, by = c("year", "stockn")) %>% 
  dplyr::mutate(value = count / ex_as,
                name = paste0(name, " Survey"),
                name_f = ifelse(stockn <= 6, "Smain", "Syent")) %>%
  dplyr::select(year, name, name_f, value) %>%
  dplyr::filter(!is.na(value))

markrecap <- mr %>%
  dplyr::mutate(value = mr_mainstem + mr_yentna,
                name = "Mark-Recapture",
                name_f = "IR",
                year = year_id) %>%
  dplyr::select(year, name, name_f, value)

indicies <- 
  rbind(weirs, surveys, markrecap) %>%
    dplyr::mutate(name_f = factor(name_f,
                                levels = c("Smain", "Syent", "IR"),
                                labels = c("Escapement-mainstem", "Escapement-Yentna", "Inriver Run")),
                  year = as.numeric(year))

stats <- stats_dat %>%
  dplyr::select_(value = as.name("50%"), lcb = as.name("2.5%"), ucb = as.name("97.5%")) %>%
  tibble::rownames_to_column() %>%
  dplyr::filter(grepl("^S\\[|^IR\\[", rowname)) %>%
  dplyr::mutate(name_f = gsub("(^.*)\\[.*", "\\1", rowname),
                name_f = ifelse(name_f == "S", "Smain", name_f))

breaks <- codes %>% 
  dplyr::filter(!(code %in% c("A", "B", "M", "N"))) %>%
  dplyr::select(-dplyr::ends_with("code")) %>%
  dplyr::mutate(name = paste0(name, " Survey")) %>%
  rbind(data.frame(name = c("Deshka Weir", "Mark-Recapture"), drainage = c("Susitna R.", "Z"))) %>%
  dplyr::arrange(drainage, name)
pal <- RColorBrewer::brewer.pal(5, "Paired")
pal2 <- c(pal[1], pal[2], pal[2], pal[3:5], pal[1:3], "black")
names(pal2) <- breaks$name
shapes <- c(17, 18, rep(17, 4), rep(19, 3), 15)
names(shapes) <- breaks$name

rbind(stats, stats[grepl("^S\\[\\d+\\]", stats$rowname), ] %>% dplyr::mutate(name_f = "Syent")) %>%
  dplyr::mutate(name_f =  factor(name_f,
                                 levels = c("Smain", "Syent", "IR"),
                                 labels = c("Escapement-mainstem", "Escapement-Yentna", "Inriver Run")),
         year = as.numeric(gsub("^.*\\[(\\d+)\\]", "\\1", rowname)) + yr0) %>%
  ggplot2::ggplot(ggplot2::aes(x = year, y = value)) +
    ggplot2::geom_line() +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lcb, ymax = ucb), inherit.aes = TRUE, alpha = 0.3) +
    ggplot2::facet_grid(name_f ~ ., scales = "free_y", switch = "y") +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::geom_jitter(data = indicies, ggplot2::aes(color = name, shape = name), size = 3, width = .3) +
    #ggplot2::geom_pointrange(data = indicies2, ggplot2::aes(ymin = lb, ymax = ub, color = "IR.hat", shape = "IR.hat")) +
    ggplot2::scale_color_manual(name ="Index",
                                breaks = breaks$name,
                                values = pal2) +
    ggplot2::scale_shape_manual(name ="Index",
                                breaks = breaks$name,
                                values = shapes) +
    ggplot2::scale_x_continuous("Year", breaks = seq(min(year_id), max(year_id), 3), minor_breaks = NULL) +
    ggplot2::scale_y_continuous(minor_breaks = NULL, labels = scales::comma) +
    ggplot2::theme_bw() +
    ggplot2::theme(strip.background = ggplot2::element_rect(colour="white", fill="white"), strip.placement = "outside")
}
