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
  lut <- data.frame(rowname = c("lnalpha",
                                "alpha",
                                "beta",
                                "phi",
                                "sigma.white",
                                "S.max",
                                "S.eq",
                                "S.msy",
                                "U.msy",
                                "D.sum",
                                "sigma.asmain",
                                "sigma.asyent",
                                "sigma.weir"),
                    Parameter = c("ln($\\alpha$)",
                              "$\\alpha$",
                              "$\\beta$",
                              "$\\phi$",
                              "$\\sigma_{w}$",
                              "$S_{MSR}$",
                              "$S_{EQ}$",
                              "$S_{MSY}$",
                              "$U_{MSY}$",
                              "D",
                              "$\\sigma_{asmain}$",
                              "$\\sigma_{asyentna}$",
                              "$\\sigma_{weir}$"),
                    stringsAsFactors = FALSE)

  stats_dat %>%
    dplyr::select_(median = as.name("50%"), q05 = as.name("5%"), q95 = as.name("95%")) %>%
    tibble::rownames_to_column() %>%
    dplyr::right_join(lut, by = "rowname") %>%
    dplyr::mutate(cv = sqrt(exp(((log(q95)-log(q05))/1.645/2)^2)-1)) %>%
    dplyr::select(Parameter, median, q05, q95, cv) %>%
    dplyr::mutate_at(c("median", "q05", "q95", "cv"), SusitnaEG:::digits) %>%
    pixiedust::dust() %>%
      pixiedust::sprinkle_colnames(median = "Median", q05 = "0.05 percentile", q95 = "0.95 percentile", cv = "CV")
}
