#' Table of SR analysis paramerater estimates
#'
#' Produces a table of paramerater estimates for the SRA.
#'
#' @param stats_dat The output from Kenai SRA::get_summary()
#'
#' @return A table
#'
#' @examples
#' table_params(get_summary(post))
#'
#' @export
table_params <- function(stats_dat){
  lut <- data.frame(rowname = c(paste0("lnalpha[", 1:5, "]"),
                                paste0("beta[", 1:5, "]"),
                                paste0("phi[", 1:5, "]"),
                                paste0("sigma.white[", 1:5, "]"),
                                paste0("S.max[", 1:5, "]"),
                                paste0("S.eq[", 1:5, "]"),
                                paste0("S.msy[", 1:5, "]"),
                                paste0("U.msy[", 1:5, "]"),
                                rep("Dsum.age", 5),
                                paste0("Dsum.S", 2:5),
                                paste0("Bsum.So[", 1:4, "]"),
                                rep("sigma.weir", 2)),
                    Parameter = factor(c(
                      rep("ln($\\alpha$)", 5),
                      rep("$\\beta$", 5),
                      rep("$\\phi$", 5),
                      rep("$\\sigma_{w}$", 5),
                      rep("$S_{MSR}$", 5),
                      rep("$S_{EQ}$", 5),
                      rep("$S_{MSY}$", 5),
                      rep("$U_{MSY}$", 5),
                      rep("$D_{age}$", 5),
                      rep("$D_{comp}$", 4),
                      rep("$B_{survey}$", 4),
                      rep("$\\sigma_{weir}$", 2)),
                      levels = c(
                        "ln($\\alpha$)", 
                        "$\\beta$", 
                        "$\\phi$", 
                        "$\\sigma_{w}$", 
                        "$\\sigma_{weir}$",
                        "$D_{age}$",
                        "$D_{comp}$",
                        "$B_{survey}$",
                        "$S_{MSR}$",
                        "$S_{EQ}$",
                        "$S_{MSY}$",
                        "$U_{MSY}$")),
                    stock = c(rep(1:5, times = 9), rep(2:5, times = 2), 1:2),
                    stringsAsFactors = FALSE)
  
  temp <-
    stats_dat %>%
    dplyr::select_(median = as.name("50%"), sd = "SD", q05 = as.name("5%"), q95 = as.name("95%")) %>%
    tibble::rownames_to_column() %>%
    dplyr::right_join(lut, by = "rowname") %>%
    dplyr::mutate(cv = ifelse(grepl("^S.", rowname),
                              sqrt(exp(((log(q95)-log(abs(q05)))/1.645/2)^2)-1), #Geometric CV for lognormals, abs(q05) to suppresses NaN warning on phi
                              sd / abs(median)),
                  stock0 = gsub("^.*\\[(\\d)\\]", "\\1", rowname)) %>%
    dplyr::mutate_at(c("median", "q05", "q95", "cv"), SusitnaEG:::digits) %>%
    dplyr::mutate(print1 = paste0(median, " (", q05, " - ", q95, ")"),
                  print2 = paste0(median, " (", cv, ")")) %>%
    dplyr::select(Parameter, print2, stock) %>%
    tidyr::spread(stock, print2)
  
  colnames(temp)  <- c("Parameter", stock_id)
  
  knitr::kable(temp, escape = FALSE)
}
