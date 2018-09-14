#' Age composition table
#'
#' Produces a table of age-at-maturity, age composition or total run by age along with sd or cv.
#'
#' @param post_dat The posterior object from the SRA model of class jagsUI
#' @param node The posterior node of interest as a character string; p(age at maturity), q(age at return) or N.ta(Number at return)
#'
#' @return A table
#'
#' @examples
#' table_age(post, "N.ta")
#'
#' @export
table_age <- function(post_dat, node){
  stopifnot(exists("year_id", .GlobalEnv),
            exists("age_max", .GlobalEnv))
  yr0 <- as.numeric(min(year_id)) - 1
  yr0_p <- yr0 - age_max
  
  mean <- get_array(post_dat, node, "mean")
  
  sd <- get_array(post_dat, node, statistic = "sd")
  
  temp <- 
    dplyr::left_join(mean, sd, by = c("yr", "age")) %>%
    dplyr::mutate(CV = sd/mean,
                  print = paste0(digits(mean), " (", digits(if(node == "N.ta") CV else sd), ")")) %>%
    dplyr::select(yr, age, print) %>%
    tidyr::spread(age, print) %>%
    dplyr::mutate_if(is.numeric, dplyr::funs(if(node == "p") {yr0_p + .} else(yr0 + .)))
  
  colnames(temp) <- c(if(node =="p") "Brood Year" else("Calendar Year"), paste0(names(age_id), " (", if(node == "N.ta") "CV)" else("sd)")))
  
  knitr::kable(temp, escape = FALSE, align = "r")
}


#' Table of Aerial Survey Standard Deviations.
#'
#' Produces a table of standard deviations for each flown tributary.
#'
#' @param post_dat The posterior object from the SRA model of class jagsUI
#'
#' @return A table
#'
#' @examples
#' table_airerror(post)
#'
#' @export
table_airerror <- function(post_dat){
  stopifnot(exists("stock_id", .GlobalEnv),
            exists("trib_id", .GlobalEnv),
            "package:SusitnaEG" %in% search())
  
  id0 <- lapply(trib_id, function(x){x[!grepl("Other", x)]})
  names(id0) <- names(as)
  id <- data.frame(stock = factor(rep(names(id0), sapply(id0, length)), levels = stock_id),
                   tribn = 1:length(unlist(id0)),
                   trib0 = unlist(id0, use.names = FALSE),
                   stringsAsFactors = FALSE) %>%
    dplyr::mutate(trib = factor(trib0, levels = trib0)) %>%
    dplyr::select(-trib0) %>%
    dplyr::arrange(stock, tribn)
  
  temp <-
    post_dat[["summary"]][grepl("sigma.air", rownames(post_dat$summary)), c("mean", "sd", "2.5%", "97.5%")]  %>% 
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    dplyr::mutate(cv = sd / mean,
                  tribn = gsub("^.*\\[(\\d+)\\]", "\\1", rowname),
                  trib = id$trib[as.numeric(tribn)]) %>%
    dplyr::mutate_if(is.numeric, digits) %>%
    dplyr::rename(q2.5 = "2.5%", q97.5 = "97.5%") %>%
    dplyr::mutate(print1 = paste0(mean, " (", q2.5, " - ", q97.5, ")"),
                  print2 = paste0(mean, " (", cv, ")")) %>%
    dplyr::select(trib, print1)
  
  colnames(temp)  <- c("Tributary", "$\\sigma_{air}$(95% CI)")
  knitr::kable(temp, escape = FALSE, align = "r")
}


#' Table of SR analysis paramerater estimates
#'
#' Produces a table of paramerater estimates for the SRA.
#'
#' @param post_dat The posterior object from the SRA model of class jagsUI
#'
#' @return A table
#'
#' @examples
#' table_params(post)
#'
#' @export
table_params <- function(post_dat, stocks = 1:4){
  stock_n <- length(stocks)
  lut <- data.frame(rowname = c(paste0("lnalpha[", stocks, "]"),
                                paste0("beta[", stocks, "]"),
                                paste0("phi[", stocks, "]"),
                                paste0("sigma.white[", stocks, "]"),
                                paste0("S.max[", stocks, "]"),
                                paste0("S.eq[", stocks, "]"),
                                paste0("S.msy[", stocks, "]"),
                                paste0("U.msy[", stocks, "]"),
                                rep("Dsum.age", stock_n),
                                if(stock_n == 1 & (1 %in%stocks)){}else{paste0("Dsum.S", intersect(stocks, 2:5))},
                                if(stock_n == 1 & (1 %in%stocks) == 1){}else{paste0("Bsum.So[", intersect(stocks - 1, 1:4), "]")},
                                rep("sigma.weir", length(intersect(stocks, 1:2)))),
                    Parameter = factor(c(rep("ln($\\alpha$)", stock_n),
                                        rep("$\\beta$", stock_n),
                                        rep("$\\phi$", stock_n),
                                        rep("$\\sigma_{w}$", stock_n),
                                        rep("$S_{MSR}$", stock_n),
                                        rep("$S_{EQ}$", stock_n),
                                        rep("$S_{MSY}$", stock_n),
                                        rep("$U_{MSY}$", stock_n),
                                        rep("$D_{age}$", stock_n),
                                        rep("$D_{comp}$", length(intersect(stocks, 2:5))),
                                        rep("$B_{survey}$", length(intersect(stocks - 1, 1:4))),
                                        rep("$\\sigma_{weir}$", length(intersect(stocks, 1:2)))),
                                      levels = c("ln($\\alpha$)", 
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
                    stock = c(rep(stocks, times = 9), rep(intersect(stocks, 2:5), times = 2), intersect(stocks, 1:2)),
                    stringsAsFactors = FALSE)
  
  temp <-
    post_dat[["summary"]][, c("50%", "sd", "2.5%", "97.5%")]  %>%
      as.data.frame() %>%
      dplyr::rename(median = "50%", q2.5 = "2.5%", q97.5 = "97.5%") %>%
      tibble::rownames_to_column() %>%
      dplyr::right_join(lut, by = "rowname") %>%
      dplyr::mutate(cv = ifelse(grepl("^S.", rowname),
                                sqrt(exp(((log(q97.5)-log(abs(q2.5)))/1.645/2)^2)-1), #Geometric CV for lognormals, abs(q05) to suppresses NaN warning on phi
                                sd / abs(median)),
                    stock0 = gsub("^.*\\[(\\d)\\]", "\\1", rowname)) %>%
      dplyr::mutate_at(c("median", "q2.5", "q97.5", "cv"), digits) %>%
      dplyr::mutate(print1 = paste0(median, " (", q2.5, " - ", q97.5, ")"),
                    print2 = paste0(median, " (", cv, ")"))
  
  if(stock_n != 1){
    print <- 
      temp %>%
      dplyr::select(Parameter, print2, stock) %>%
      tidyr::spread(stock, print2)
    colnames(print) <- c("Parameter", stock_id[stocks])} 
  else{
    print <- 
      temp %>%
      dplyr::select(Parameter, print1, stock) %>%
      tidyr::spread(stock, print1)
    colnames(print) <- c("Parameter", "median(95% CI)")}
  
  knitr::kable(print, escape = FALSE, align = "r")
}


#' State Variable Table
#'
#' Produces a table of escapement, recruitment, total run, and inriver run along with cv's.
#'
#' @param post_dat The posterior object from the SRA model of class jagsUI
#' @param display "bystock" for a list of state parameter estimates for each stock and "bystate" for 
#'  a list of stock parameter estimates for each state.
#'
#' @return A table
#'
#' @examples
#' table_state(post, "bystate")
#'
#' @export
table_state <- function(post_dat, display){
  stopifnot(exists("year_id", .GlobalEnv),
            exists("stock_id", .GlobalEnv),
            exists("age_max", .GlobalEnv))
  yr0 <- as.numeric(min(year_id)) - 1
  yr0_R <- yr0 - age_max
  
  temp <- 
    post_dat[["summary"]] %>%
    as.data.frame() %>%
    dplyr::select(mean, sd, median = "50%") %>%
    tibble::rownames_to_column() %>%
    dplyr::filter(grepl("^R\\[|S\\[|N\\[|IR\\[", rowname)) %>%
    dplyr::mutate(name = factor(gsub("^(.*)\\[\\d+,\\d\\]", "\\1", rowname),
                                levels = c("N", "IR", "S", "R"),
                                labels = c("Total Run", "Inriver Run", "Escapement", "Recruitment")),
                  index = as.numeric(gsub("^.*\\[(\\d+),\\d\\]", "\\1", rowname)),
                  year = (name != c("Recruitment")) * (yr0 + index) + (name == "Recruitment") * (yr0_R + index),
                  stock = factor(stock_id[gsub("^.*\\[\\d+,(\\d)\\]", "\\1", rowname)], levels = stock_id),
                  cv = sd/mean,
                  print = paste0(format(round(median, 0), big.mark = ","), " (", format(round(cv, 2), nsmall = 2), ")"))
  
  if(display == "bystock"){
    temp2 <-
      temp %>%
      dplyr::select(year, name, stock, print) %>%
      tidyr::spread(name, print) %>%
      dplyr::rowwise() %>%
      dplyr::mutate_all(nareplace) %>%
      dplyr::arrange(stock, year)
   
    out <- split(temp2, temp2$stock) %>% knitr::kable(row.names = FALSE, align = "r", escape = FALSE)
  }

  if(display == "bystate"){
    temp3 <-
      temp %>%
      dplyr::select(year, name, stock, print) %>%
      tidyr::spread(stock, print) %>%
      dplyr::select(year, name, Deshka, 'East Susitna', Talkeetna, Yentna, Other) %>%
      dplyr::rowwise() %>%
      dplyr::mutate_all(nareplace) %>%
      dplyr::arrange(name, year)
    
    out <- split(temp3, temp3$name) %>% knitr::kable(row.names = FALSE, align = "r", escape = FALSE)
  }
  out
}


#' Stock composition table
#'
#' Produces a table of stock composition by drainage along with cv's.
#'
#' @param post_dat The posterior object from the SRA model of class jagsUI
#'
#' @return A table
#'
#' @examples
#' table_stock(post)
#'
#' @export
table_stock <- function(post_dat){
  stopifnot(exists("year_id", .GlobalEnv),
            exists("age_max", .GlobalEnv),
            exists("stock_id", .GlobalEnv),
            exists("trib_id", .GlobalEnv),
            "package:SusitnaEG" %in% search())
  yr0 <- as.numeric(min(year_id)) - 1
  yr0_R <- yr0 - age_max
  
  id <- data.frame(stock = factor(rep(names(trib_id), sapply(trib_id, length)), levels = stock_id), 
                   tribn = unlist(sapply(trib_id, function(x) 1:length(x)), use.names = FALSE), 
                   trib = factor(unlist(trib_id, use.names = FALSE), levels = unlist(trib_id, use.names = FALSE)),
                   stringsAsFactors = FALSE)
  
  est <- 
    post_dat[["summary"]][grepl("p.S", rownames(post$summary)), c("mean", "sd")] %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    dplyr::mutate(year = as.numeric(gsub("^.*\\[(\\d+).*$", "\\1", rowname)) + yr0,
                  stock = factor(unname(stock_id[gsub("p.S(\\d).*", "\\1", rowname)]), levels = stock_id),
                  tribn = as.numeric(gsub("^.*,(\\d)]$", "\\1", rowname))) %>%
    dplyr::left_join(id, by = c("stock", "tribn")) %>%
    dplyr::mutate(print = paste0(digits(mean), " (", digits(sd), ")")) %>%
    dplyr::select(stock, year, trib, print) 
  
  list <- lapply(stock_id[2:5], function(x) est[est$stock == x, ] %>%
                   dplyr::select(-stock) %>%
                   tidyr::spread(trib, print))
  
  names(list) <- stock_id[2:5]
  list
}
