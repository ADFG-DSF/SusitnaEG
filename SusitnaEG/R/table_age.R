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
  stopifnot(exists("year_id", .GlobalEnv),
            exists("age_max", .GlobalEnv))
  yr0 <- as.numeric(min(year_id)) - 1
  yr0_p <- yr0 - age_max
  
  mean <- get_array(stats_dat, node, "Mean") %>%
    tidyr::gather(age, mean, dplyr::starts_with("age"))
  
  sd <- get_array(stats_dat, node, statistic = "SD") %>%
    tidyr::gather(age, sd, dplyr::starts_with("age"))
  
  yname <- names(mean)[grepl("year", names(mean))]
  
  temp <- 
    dplyr::left_join(mean, sd, by = c(yname, "age")) %>%
    dplyr::mutate(CV = sd/mean,
                  print = paste0(SusitnaEG:::digits(mean), " (", SusitnaEG:::digits(if(node == "N.ta") CV else sd), ")")) %>%
    dplyr::select(which(grepl(paste0(yname, "|age|print"), names(.)))) %>%
    tidyr::spread(age, print) %>%
    dplyr::mutate_if(is.numeric, dplyr::funs(if(node == "p") {yr0_p + .} else(yr0 + .)))
  
  colnames(temp) <- c(if(yname =="cyear") "Calendar Year" else("Brood Year"), paste0(names(age_id), " (", if(node == "N.ta") "CV)" else("sd)")))
  
  temp %>% pixiedust::dust()
}
