#' Stock composition table
#'
#' Produces a table of stock composition by drainage along with cv's.
#'
#' @param stats_dat stats_dat The output from get_summary() for the SRA model mcmc.list ouput
#'
#' @return A table
#'
#' @examples
#' table_stock(get_summary(post))
#'
#' @export
table_stock <- function(stats_dat){
  stopifnot(exists("year_id", .GlobalEnv),
            exists("age_max", .GlobalEnv),
            exists("stock_id", .GlobalEnv),
            "package:SusitnaEG" %in% search())
  yr0 <- as.numeric(min(year_id)) - 1
  yr0_R <- yr0 - age_max
  
  id0 <- lapply(1:5, function(x) colnames(as[[x]]))
  names(id0) <- names(as)
  id <- data.frame(stock = factor(rep(names(id0), sapply(id0, length)), levels = stock_id), 
                   tribn = unlist(sapply(id0, function(x) 1:length(x)), use.names = FALSE), 
                   trib = unlist(id0, use.names = FALSE),
                   stringsAsFactors = FALSE)

  est <- stats_dat %>%
    tibble::rownames_to_column() %>%
    dplyr::filter(grepl("p.S", rowname)) %>%
    dplyr::mutate(year = as.numeric(gsub("^.*\\[(\\d+).*$", "\\1", rowname)) + yr0,
                  stock = factor(unname(stock_id[gsub("p.S(\\d).*", "\\1", rowname)]), levels = stock_id),
                  tribn = as.numeric(gsub("^.*,(\\d)]$", "\\1", rowname))) %>%
    dplyr::left_join(id, by = c("stock", "tribn")) %>%
    dplyr::mutate(print = paste0(SusitnaEG:::digits(Mean), " (", SusitnaEG:::digits(SD), ")"),
                  trib = factor(ifelse(is.na(trib), "Other", trib), levels = c(id$trib, "Other"))) %>%
    dplyr::select(stock, year, trib, print) 
  
  list <- lapply(stock_id[2:5], function(x) est[est$stock == x, ] %>%
                                              dplyr::select(-stock) %>%
                                              tidyr::spread(trib, print))
  
  names(list) <- stock_id[2:5]
  list
}
