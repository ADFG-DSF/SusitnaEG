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
                  print = paste0(digits(mean), " (", digits(if(node == "N.ta") CV else sd), ")")) %>%
    dplyr::select(which(grepl(paste0(yname, "|age|print"), names(.)))) %>%
    tidyr::spread(age, print) %>%
    dplyr::mutate_if(is.numeric, dplyr::funs(if(node == "p") {yr0_p + .} else(yr0 + .)))
  
  colnames(temp) <- c(if(yname =="cyear") "Calendar Year" else("Brood Year"), paste0(names(age_id), " (", if(node == "N.ta") "CV)" else("sd)")))
  
  temp %>% pixiedust::dust()
}


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
    dplyr::mutate_at(c("median", "q05", "q95", "cv"), digits) %>%
    dplyr::mutate(print1 = paste0(median, " (", q05, " - ", q95, ")"),
                  print2 = paste0(median, " (", cv, ")")) %>%
    dplyr::select(trib, print1)
  
  colnames(temp)  <- c("Tributary", "$\\sigma_{weir}$(90% CI)")
  knitr::kable(temp, escape = FALSE)
}


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
    dplyr::mutate_at(c("median", "q05", "q95", "cv"), digits) %>%
    dplyr::mutate(print1 = paste0(median, " (", q05, " - ", q95, ")"),
                  print2 = paste0(median, " (", cv, ")")) %>%
    dplyr::select(Parameter, print2, stock) %>%
    tidyr::spread(stock, print2)
  
  colnames(temp)  <- c("Parameter", stock_id)
  
  knitr::kable(temp, escape = FALSE)
}


#' State Variable Table
#'
#' Produces a table of escapement, recruitment, total run, and inriver run along with cv's.
#'
#' @param stats_dat The output from get_summary() for the SRA model mcmc.list output
#'
#' @return A table
#'
#' @examples
#' table_state(get_summary(post_er))
#'
#' @export
table_state <- function(stats_dat){
  stopifnot(exists("year_id", .GlobalEnv),
            exists("age_max", .GlobalEnv))
  yr0 <- as.numeric(min(year_id)) - 1
  yr0_R <- yr0 - age_max
  
  stats_dat %>%
    dplyr::select_(median = as.name("50%"), mean = "Mean", sd = "SD") %>%
    tibble::rownames_to_column() %>%
    dplyr::filter(grepl("^R\\[|S\\[|N\\[|IR\\[", rowname)) %>%
    dplyr::mutate(name = stringr::str_sub(rowname, 1, stringr::str_locate(rowname, "\\[")[, 1] - 1),
                  index = as.numeric(stringr::str_sub(rowname, stringr::str_locate(rowname, "[0-9]+"))),
                  year = (name != c("R")) * (yr0 + index) + (name == "R") * (yr0_R + index),
                  cv = sd/mean,
                  print = paste0(format(round(median, 0), big.mark = ","), " (", format(round(cv, 2), nsmall = 2), ")")) %>%
    dplyr::select(year, print, name) %>%
    tidyr::spread(name, print) %>%
    dplyr::select(year, N, IR, S, R) %>%
    pixiedust::dust(justify = "right") %>%
    pixiedust::sprinkle_colnames(year = "Year", N = "Total Run (CV)", IR = "Inriver Run (CV)", S = "Escapement (CV)", R = "Recruitment (CV)") %>%
    pixiedust::sprinkle(fn = quote(nareplace(value)))
}


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
    dplyr::mutate(print = paste0(digits(Mean), " (", digits(SD), ")"),
                  trib = factor(ifelse(is.na(trib), "Other", trib), levels = c(id$trib, "Other"))) %>%
    dplyr::select(stock, year, trib, print) 
  
  list <- lapply(stock_id[2:5], function(x) est[est$stock == x, ] %>%
                   dplyr::select(-stock) %>%
                   tidyr::spread(trib, print))
  
  names(list) <- stock_id[2:5]
  list
}
