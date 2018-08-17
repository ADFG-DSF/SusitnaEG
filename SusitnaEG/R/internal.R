#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#format tables with numbers of very different magnitude
digits <- function(p){
  ps <- ifelse(p < 0.01, format(p, TRUE, digits = 2, scientific = TRUE),
               ifelse(p < 2, format(round(p, 2), TRUE, nsmall = 2),
                      ifelse(p < 100, format(round(p, 1), TRUE, nsmall = 1),
                             format(round(p, 0), TRUE, nsmall = 0, width = 5, scientific = FALSE, big.mark = ","))))
  return(ps)
}

# Used to replace NA with a single dash in table_state.
nareplace <- function(value){if(stringr::str_detect(value, "NA") | is.na(value)) "-" else value}

#Capitalize first letter
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

#Scale Statewide BEGs bounds by Smsy for plotting
get_BEGbounds <- function(scale_Smsy){
  chinBEGs %>% dplyr::mutate(lb = scale_Smsy*lb/Smsy,
                             ub = scale_Smsy*ub/Smsy)
}

#used by plot_age and table_age
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