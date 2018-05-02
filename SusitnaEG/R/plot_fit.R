#' Early Run Model fit plots
#'
#' Produces a faceted plot of inriver, midriver and total run with the appropriately scaled indices of abundance that were used as inputs to the model.
#'
#' @param input_dat The input dataset for the SRA model
#' @param stats_dat The output from get_summary() for the SRA model mcmc.list output
#'
#' @return A figure
#'
#' @examples
#' plot_ERfit(dat_erinput, get_summary(post_er))
#'
#' @export
plot_ERfit <- function(input_dat, stats_dat){
qhat <- stats_dat %>%
  dplyr::as.tbl() %>%
  tibble::rownames_to_column() %>%
  dplyr::filter(grepl("^q.i\\[", rowname)) %>%
  dplyr::select(rowname, q = Mean) %>%
  dplyr::mutate(index_name = ifelse(rowname == "q.i[1]", "ncpue", ifelse(rowname == "q.i[2]", "nasb", ifelse(rowname =="q.i[3]", "scpue", "NhatLR"))))

indicies1 <- input_dat %>% 
  dplyr::select(ncpue, nasb, scpue, NhatLR, DLge75, ALge75) %>%
  tibble::rownames_to_column(var = "year") %>%
  dplyr::mutate(year = 1985 + as.numeric(year)) %>%
  tidyr::gather(index_name, raw, -year) %>%
  dplyr::left_join(qhat, by = "index_name") %>%
  dplyr::mutate(value = ifelse(!is.na(q), raw/q, raw),
         name = factor(index_name,
                       levels = c("ncpue", "nasb", "DLge75", "scpue", "IR.hat", "ALge75", "NhatLR"),
                       labels = c(rep("Midriver Run", 3), rep("Inriver Run", 3), "Total Run")))

indicies2 <- input_dat %>%
  dplyr::select(value = IR.hat, cv.IR) %>%
  tibble::rownames_to_column(var = "year") %>%
  dplyr::mutate(year = 1985 + as.numeric(year),
         name = "Inriver Run",
         ub = exp(log(value) + 1.96 * sqrt(log(cv.IR * cv.IR + 1))),
         lb = exp(log(value) - 1.96 * sqrt(log(cv.IR * cv.IR + 1))))

stats_dat %>%
  dplyr::select_(value = as.name("50%"), lcb = as.name("2.5%"), ucb = as.name("97.5%")) %>%
  tibble::rownames_to_column() %>%
  dplyr::filter(grepl("^Midriver.Run\\[|Inriver.Run\\[|N\\[", rowname)) %>%
  dplyr::mutate(name = factor(stringr::str_sub(rowname, 1, stringr::str_locate(rowname, "\\[")[, 1] - 1),
                       levels = c("Midriver.Run", "Inriver.Run", "N"),
                       labels = c("Midriver Run", "Inriver Run", "Total Run")),
         index = as.numeric(stringr::str_sub(rowname, stringr::str_locate(rowname, "[0-9]+"))),
         year = (1985 + index)) %>%
  ggplot2::ggplot(ggplot2::aes(x = year, y = value)) +
    ggplot2::geom_line() +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lcb, ymax = ucb), inherit.aes = TRUE, alpha = 0.3) +
    ggplot2::facet_grid(name ~ ., scales = "free_y", switch = "y") +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::coord_cartesian(xlim = c(1986, 2015)) +
    ggplot2::geom_jitter(data = indicies1, ggplot2::aes(color = index_name, shape = index_name), size = 3, width = .3) +
    ggplot2::geom_pointrange(data = indicies2, ggplot2::aes(ymin = lb, ymax = ub, color = "IR.hat", shape = "IR.hat")) +
    ggplot2::scale_color_manual(name ="Index",
                                breaks = c("ALge75", "IR.hat", "scpue", "DLge75", "nasb", "ncpue", "NhatLR"),
                                values = c("ALge75" = "#e41a1c",
                                           "IR.hat" = "#377eb8",
                                           "scpue" = "#4daf4a",
                                           "DLge75" = "#e41a1c",
                                           "nasb" = "#377eb8",
                                           "ncpue" = "#4daf4a",
                                           "NhatLR" = "#e41a1c"),
                                labels = c("ARIS", "CR", "SCPUE", "DIDSON", "NASB", "NCPUE", expression(paste(N[LR])))) +
    ggplot2::scale_shape_manual(name ="Index",
                                breaks = c("ALge75", "IR.hat", "scpue", "DLge75", "nasb", "ncpue", "NhatLR"),
                                values = c("ALge75" = 17,
                                           "IR.hat" = 17,
                                           "scpue" = 17,
                                           "DLge75" = 18,
                                           "nasb" = 18,
                                           "ncpue" = 18,
                                           "NhatLR" = 15),
                                labels = c("ARIS", "CR", "SCPUE", "DIDSON", "NASB", "NCPUE", expression(paste(N[LR])))) +
    ggplot2::scale_x_continuous("Year", breaks = seq(1985, 2015, 3), minor_breaks = NULL) +
    ggplot2::scale_y_continuous(minor_breaks = NULL, labels = scales::comma) +
    ggplot2::theme_bw() +
    ggplot2::theme(strip.background = ggplot2::element_rect(colour="white", fill="white"), strip.placement = "outside")
}
