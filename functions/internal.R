#' Pipe
#'
#' Use the pipe function \code{\%>\%} to turn function composition into a series of imperative statements.
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @export
#' @param lhs,rhs A object and a function to apply to it
NULL

#format tables with numbers of very different magnitude
digits <- function(p){
  ps <- ifelse(p == 0 , format(p, TRUE, digits = 0),
               ifelse(abs(p) < 0.01, format(p, TRUE, digits = 2, scientific = TRUE),
                      ifelse(abs(p) < 2, format(round(p, 2), TRUE, nsmall = 2),
                             ifelse(abs(p) < 100, format(round(p, 1), TRUE, nsmall = 1),
                                    format(round(p, 0), TRUE, nsmall = 0, width = 5, scientific = FALSE, big.mark = ",")))))
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
get_array <- function(post_dat, node, statistic = "mean"){
  pattern <- paste0("^", node, "\\[")
  df <- 
    post_dat[["summary"]] %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    dplyr::filter(grepl(pattern, rowname)) %>%
    dplyr::mutate(yr = as.numeric(gsub(".*\\[(\\d+),\\d]", "\\1", rowname)),
                  age = as.numeric(gsub(".*\\[\\d+,(\\d)]", "\\1", rowname))) %>%
    dplyr::select_("yr", "age", statistic)
    df
}
