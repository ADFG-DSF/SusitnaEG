#' State Variable Table
#'
#' Produces a table of escapement, recruitment, total run, and inriver run along with cv's.
#'
#' @param stats_dat The output from get_summary() for the SRA model mcmc.list output
#'
#' @return A table
#'
#' @examples
#' table_state(get_summary(post_er))
#'
#' @export
table_state <- function(stats_dat){
  stats_dat %>%
    dplyr::select_(median = as.name("50%"), mean = "Mean", sd = "SD") %>%
    tibble::rownames_to_column() %>%
    dplyr::filter(grepl("^R\\[|S\\[|N\\[|IR\\[", rowname)) %>%
    dplyr::mutate(name = stringr::str_sub(rowname, 1, stringr::str_locate(rowname, "\\[")[, 1] - 1),
                  index = as.numeric(stringr::str_sub(rowname, stringr::str_locate(rowname, "[0-9]+"))),
                  year = (name != c("R")) * (1978 + index) + (name == "R") * (1978 - 6 + index),
                  cv = sd/mean,
                  print = paste0(format(round(median, 0), big.mark = ","), " (", format(round(cv, 2), nsmall = 2), ")")) %>%
    dplyr::select(year, print, name) %>%
    tidyr::spread(name, print) %>%
    dplyr::select(year, N, IR, S, R) %>%
    pixiedust::dust(justify = "right") %>%
      pixiedust::sprinkle_colnames(year = "Year", N = "Total Run (CV)", IR = "Inriver Run (CV)", S = "Escapement (CV)", R = "Recruitment (CV)") %>%
      pixiedust::sprinkle(fn = quote(KenaiSRA:::nareplace(value)))
}
