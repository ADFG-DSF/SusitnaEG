#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#format tables with numbers of very different magnitude
digits <- function(p){
  ps <- ifelse(p < 0.01, format(p, TRUE, digits = 3, scientific = TRUE),
               ifelse(p < 2, format(round(p, 2), TRUE, nsmall = 2),
                      ifelse(p < 100, format(round(p, 1), TRUE, nsmall = 1),
                             format(round(p, 0), TRUE, nsmall = 0, width = 5, scientific = FALSE, big.mark = ","))))
  return(ps)
}