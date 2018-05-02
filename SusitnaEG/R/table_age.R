#' Age composition table
#'
#' Produces a table of age-at-maturity, age composition or total run by age along with sd or cv.
#'
#' @param stats_dat stats_dat The output from get_summary() for the SRA model mcmc.list ouput
#' @param node The posterior node of interest as a character string; p(age at maturity), q(age at return) or N.ta(Number at return)
#'
#' @return A table
#'
#' @examples
#' table_age(get_summary(post), "N.ta")
#'
#' @export
table_age <- function(stats_dat, node){
  mean <- get_array(stats_dat, node, "Mean") %>%
    tidyr::gather(age, mean, dplyr::starts_with("age"))
  
  sd <- get_array(stats_dat, node, statistic = "SD") %>%
    tidyr::gather(age, sd, dplyr::starts_with("age"))
  
  yname <- names(mean)[grepl("year", names(mean))]
  
  dplyr::left_join(mean, sd, by = c(yname, "age")) %>%
    dplyr::mutate(CV = sd/mean,
                  print = paste0(SusitnaEG:::digits(mean), " (", SusitnaEG:::digits(if(node == "N.ta") CV else sd), ")")) %>%
    dplyr::select(which(grepl(paste0(yname, "|age|print"), names(.)))) %>%
    tidyr::spread(age, print) %>%
    dplyr::mutate_if(is.numeric, dplyr::funs(if(node == "p") {. - 6 + 1978} else(. + 1978))) %>%
    pixiedust::dust() %>%
    pixiedust::sprinkle_colnames(if(yname =="cyear") "Calendar Year" else("Brood Year"),
                                   paste0("Age-34 (", if(node == "N.ta") "CV)" else("sd)")),
                                   paste0("Age-5 (", if(node == "N.ta") "CV)" else("sd)")),
                                   paste0("Age-678 (", if(node == "N.ta") "CV)" else("sd)")))
}
