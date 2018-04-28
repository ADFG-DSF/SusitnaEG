#' Total Run by age table
#'
#' Produces a table of total run by age along with cv's.
#'
#' @param stats_dat The output from get_summary() for the SRA model mcmc.list output
#'
#' @return A table
#'
#' @examples
#' table_age(get_summary(post_er))
#'
#' @export
table_age <- function(stats_dat){
  stats_dat %>%
    tibble::rownames_to_column() %>%
    dplyr::filter(grepl("^N.ta\\[", rowname)) %>%
    dplyr::select_(rowname = "rowname", median = as.name("50%"), mean = "Mean", sd = "SD") %>%
    dplyr::mutate(CV = sd/mean,
                  print = paste0(format(median, digits = 0, big.mark = ",", scientific = FALSE), " (", format(CV, trim = TRUE, digits = 2), ")")) %>%
    tidyr::separate(rowname, into = c("year", "age"), ",") %>%
    dplyr::select(year, age, print) %>%
    dplyr::mutate(year = as.numeric(gsub("[^0-9]", "", year)) + 1978,
                  age = paste0("age",gsub("[^0-9]", "", age))) %>%
    tidyr::spread(age, print) %>%
    pixiedust::dust() %>%
    pixiedust::sprinkle_colnames("Year", "Age-34 (CV)", "Age-5 (CV)", "Age-678 (CV)")
}
