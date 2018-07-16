#' Table of Aerial Survey Standard Deviations.
#'
#' Produces a table of standard deviations for each flown tributary.
#'
#' @param stats_dat The output from Kenai SRA::get_summary()
#'
#' @return A table
#'
#' @examples
#' table_airerror(get_summary(post))
#'
#' @export
table_airerror <- function(stats_dat){
  stopifnot(exists("stock_id", .GlobalEnv),
            "package:SusitnaEG" %in% search())
  
  id0 <- lapply(1:5, function(x) c(colnames(as[[x]])))
  names(id0) <- names(as)
  id <- data.frame(stock = factor(rep(names(id0), sapply(id0, length)), levels = stock_id),
                   tribn = 1:length(unlist(id0)),
                   trib0 = unlist(id0, use.names = FALSE),
                   stringsAsFactors = FALSE) %>%
    dplyr::mutate(trib = factor(trib0, levels = trib0)) %>%
    dplyr::select(-trib0) %>%
    dplyr::arrange(stock, tribn)

  temp <- 
    stats_dat %>%
    dplyr::select_(median = as.name("50%"), sd = "SD", q05 = as.name("5%"), q95 = as.name("95%")) %>%
    tibble::rownames_to_column() %>%
    dplyr::filter(grepl("sigma.air", rowname)) %>%
    dplyr::mutate(cv = sd / median,
                  tribn = gsub("^.*\\[(\\d+)\\]", "\\1", rowname),
                  trib = id$trib[as.numeric(tribn)]) %>%
    dplyr::mutate_at(c("median", "q05", "q95", "cv"), SusitnaEG:::digits) %>%
    dplyr::mutate(print1 = paste0(median, " (", q05, " - ", q95, ")"),
                  print2 = paste0(median, " (", cv, ")")) %>%
    dplyr::select(trib, print1)
  
  colnames(temp)  <- c("Tributary", "$\\sigma_{weir}$(90% CI)")
  knitr::kable(temp, escape = FALSE)
}
