#' Collect posterior stats for 2d arrays
#'
#' Creates an array of age structured quantities from the output of get_summary().
#'
#' @param stats_dat stats_dat The output from get_summary() for the SRA model mcmc.list ouput
#' @param node The posterior node of interest; p(age at maturity), q(age at return) or N.ta(Number at return)
#' @param statistic defaults to "Mean" but can specify other quantities: "SD", as.name("50\%")
#'
#' @return data frame with rows of years and columns of ages
#'
#' @examples
#' get_array(get_summary(post_er), "p")
#'
#' @export
get_array <- function(stats_dat, node, statistic = "Mean"){
  pattern <- paste0("^", node, "\\[")
  df <- tibble::rownames_to_column(stats_dat) %>%
    dplyr::filter(grepl(pattern, rowname)) %>%
    tidyr::separate(rowname, into = c("year", "age"), ",") %>%
    dplyr::select_("year", "age", stat = statistic) %>%
    dplyr::mutate(year = as.numeric(gsub("[^0-9]", "", year)),
                  age = paste0("age",gsub("[^0-9]", "", age))) %>%
    tidyr::spread_("age", "stat")
  yname <- ifelse(node == "p", "byear", "cyear")
  colnames(df)[colnames(df) == 'year'] <- yname
  df
}


