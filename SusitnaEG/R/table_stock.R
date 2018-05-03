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
  id <- codes[["name"]][-1]
  
  fork <- stats_dat[grepl("^pf.main", rownames(stats_dat)), "Mean"] %>%
    dplyr::rename(main = Mean) %>%
    dplyr::mutate(year = 1979:2017,
                  yent = 1 - main)

  stock <- function(node){
    stats_dat %>%
      tibble::rownames_to_column() %>%
      dplyr::filter(grepl(paste0(node, "\\["), rowname)) %>%
      dplyr::mutate(year = as.numeric(gsub("^.*\\[(\\d+).*$", "\\1", rowname)) + 1978,
                    stock0 = as.numeric(gsub("^.*,(\\d)]$", "\\1", rowname)),
                    node = node,
                    stockn = ifelse(node == "pm", stock0, stock0 + 6),
                    stock = id[stockn]) %>%
      dplyr::select(year, stockn, stock, p = Mean, SD)
  }

  Susitna <- 
    dplyr::left_join(rbind(stock("pm"), stock("py")), fork, by = "year") %>%
      dplyr::mutate(p = ifelse(stockn <= 6, p * main, p * yent),
                    print = paste0(SusitnaEG:::digits(p), " (", SusitnaEG:::digits(SD), ")")) %>%
      dplyr::select(-p, -SD, -stockn, -main, -yent) %>%
      tidyr::spread(stock, print)
   
  Mainstem <-
     stock("pm") %>%
       dplyr::mutate(print = paste0(SusitnaEG:::digits(p), " (", SusitnaEG:::digits(SD), ")")) %>%
       dplyr::select(-p, -SD, -stockn) %>%
       tidyr::spread(stock, print)

  Yetna <-
   stock("py") %>%
     dplyr::mutate(print = paste0(SusitnaEG:::digits(p), " (", SusitnaEG:::digits(SD), ")")) %>%
     dplyr::select(-p, -SD, -stockn) %>%
     tidyr::spread(stock, print)
  
  list(Susitna, Mainstem, Yetna)
}
