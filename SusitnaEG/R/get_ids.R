#' Lookup tables for year and age
#'
#' Creates lookup tables to switch between informative names and jags array locations.
#'
#' @param year_range character vector with the indices of abundance included in the analysis.
#' @param age_id named vector of beta names and beta column position.
#'
#' @return writes 3 objects to R's Global Environment; year_id and age_id where each object is a named vector where the names are informative
#' and the elements are jags array locations. Also, age_min and age_max the number of years between spawning and recruitment for the youngest and
#' oldest non-negligible age classes.
#'
#' @examples
#' get_ids(1979:2017)
#'
#' @export
get_ids <- function(year_range = 1979:2017,
                    age_id = c("age34" = 1, "age5" = 2, "age678" = 3)){
  age_min <- as.numeric(gsub("^age.*(\\d$)", "\\1", names(age_id)[1]))
  age_max <- as.numeric(gsub("^age(\\d).*", "\\1", names(age_id)[3]))

  year_id <- as.character(year_range)
  names(year_id) <- 1:length(year_range)

  list <- list(year_id = year_id,
               age_id = age_id,
               age_min = age_min,
               age_max = age_max)
  list2env(list, .GlobalEnv)
}
