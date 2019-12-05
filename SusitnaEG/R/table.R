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
            exists("age_max", .GlobalEnv),
            exists("age_id", .GlobalEnv),
            exists("age_min", .GlobalEnv))
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
  
  colnames(temp) <- c(if(node =="p") "Brood year" else("Calendar year"), 
                      paste0(names(age_id), if(node == "N.ta") " (CV)" else(" (SD)")))
  
  knitr::kable(temp, escape = FALSE, align = "r")
}


#' Table of Aerial Survey Observability and Standard Deviations.
#'
#' Produces a table of air survey observability and standard deviations for each flown tributary.
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
    post_dat[["summary"]][grepl("sigma.air|theta\\[\\d+,1\\]", rownames(post_dat$summary)), c("50%", "sd", "2.5%", "97.5%")]  %>% 
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    dplyr::mutate(param = gsub("(^.*)\\[.*", "\\1", rowname),
                  tribn = ifelse(param == "sigma.air", 
                                 gsub("^.*\\[(\\d+)\\]", "\\1", rowname),  
                                 gsub("^.*\\[(\\d+).*\\]", "\\1", rowname)),
                  trib = id$trib[as.numeric(tribn)],
                  stock = id$stock[as.numeric(tribn)]) %>%
    dplyr::mutate_if(is.numeric, digits) %>%
    dplyr::rename(median = "50%", q2.5 = "2.5%", q97.5 = "97.5%") %>%
    dplyr::mutate(print1 = paste0(median, " (", q2.5, "\U{2013}", q97.5, ")")) %>%
    dplyr::select(stock, trib, param, print1) %>%
    tidyr::spread(param, print1) %>%
    dplyr::select(stock, trib, theta, sigma.air)
  
  colnames(temp)  <- c("Stock", "Population", "$\\theta_i$(95% CI)", "$\\sigma_{ASi}$(95% CI)")
  knitr::kable(temp, escape = FALSE, align = "r")
}

#' Brood table
#'
#' Produces an brood tables based on the posterior means form the state space model.  Most of the table functions in this package are intended
#'  for output into word via markdown.  This one produces a tibble object that can be output to excel using WriteXLS::WriteXLS.
#'
#' @param post_dat The posterior object from the SRA model of class jagsUI
#' @param stock numeric. See stock_id for numeric codes.
#'
#' @return A tibble
#'
#' @examples
#' table_brood(post_dat, stock)
#'
#' @export
table_brood <- function(post_dat, stock){
  stopifnot(exists("year_id", .GlobalEnv))
    
  N_ta <- #q50 because when written N.tas was calculated from p and R posterior sims and then ammended JagsUI object   
    data.frame(mean = post_dat$mean$N.tas[, , stock],
               cyr = as.numeric(year_id[1]) - 1 + as.vector(slice.index(post_dat$mean$N.tas[, 1, stock], 1))) %>%
    tidyr::gather(key, mean, -cyr) %>%
    dplyr::mutate(age_n = as.numeric(gsub(".*\\.(\\d)", "\\1", key)) + post_dat$data$a.min - 1,
                  year = cyr - age_n,
                  age_c = paste0("age", age_n)) %>%
    dplyr::select(year, age_c, mean) %>%
    tidyr::spread(age_c, mean)
    
  post_dat[["summary"]][grepl(paste0("^S\\[\\d+,", stock, "|^R\\[\\d+,", stock), rownames(post_dat$summary)), c("mean", "50%")] %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    dplyr::mutate(name = stringr::str_sub(rowname, 1, stringr::str_locate(rowname, "\\[")[, 1] - 1),
                  index = as.numeric(gsub(".\\[(\\d+),\\d]", "\\1", rowname)),
                  year = (name == "S") * (as.numeric(year_id[1]) - 1 + index) + (name == "R") * (as.numeric(year_id[1]) - 1 - post_dat$data$a.max + index)) %>%
    dplyr::select(year, mean, name) %>%
    tidyr::spread(name, mean) %>%
    dplyr::select(year, S, R) %>%
    dplyr::full_join(N_ta, by = "year") %>%
    dplyr::mutate_all(as.integer)
}

#' Table of SR analysis paramerater estimates
#'
#' Produces a table of paramerater estimates for the SRA.
#'
#' @param post_dat The posterior object from the SRA model of class jagsUI
#' @param stocks Numeric, stock numbers to include in the table
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
                                if(stock_n == 1 & (1 %in%stocks) == 1){}else{paste0("Bsum.So[", intersect(stocks - 1, 1:4), "]")}),
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
                                        rep("$B_{survey}$", length(intersect(stocks - 1, 1:4)))),
                                      levels = c("ln($\\alpha$)", 
                                                "$\\beta$", 
                                                "$\\phi$", 
                                                "$\\sigma_{w}$",
                                                "$D_{age}$",
                                                "$D_{comp}$",
                                                "$B_{survey}$",
                                                "$S_{MSR}$",
                                                "$S_{EQ}$",
                                                "$S_{MSY}$",
                                                "$U_{MSY}$")),
                    stock = c(rep(stocks, times = 9), rep(intersect(stocks, 2:5), times = 2)),
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
      dplyr::mutate(print1 = paste0(median, " (", q2.5, "\U{2013}", q97.5, ")"),
                    print2 = paste0(median, " (", cv, ")"))
  
  print <- 
    temp %>%
    dplyr::select(Parameter, print1, stock) %>%
    tidyr::spread(stock, print1)
    colnames(print) <- c("Parameter", paste0(stock_id[stocks], " (95% CI)"))
  
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
                                labels = c("Total run (CV)", "Inriver run (CV)", "Escapement (CV)", "Recruitment (CV)")),
                  index = as.numeric(gsub("^.*\\[(\\d+),\\d\\]", "\\1", rowname)),
                  year = ifelse(gsub("^(.*)\\[\\d+,\\d\\]", "\\1", rowname) == "R", (yr0_R + index), (yr0 + index)),
                  stock = factor(stock_id[gsub("^.*\\[\\d+,(\\d)\\]", "\\1", rowname)], 
                                 levels = stock_id,
                                 labels = paste0(stock_id, " (CV)")),
                  cv = sd/mean,
                  print = paste0(format(round(median, 0), big.mark = ","), " (", format(round(cv, 2), nsmall = 2), ")"))
  
  if(display == "bystock"){
    temp2 <-
      temp %>%
      dplyr::select(year, name, stock, print) %>%
      tidyr::spread(name, print) %>%
      dplyr::rowwise() %>%
      dplyr::mutate_all(nareplace) %>%
      dplyr::arrange(stock, year) %>%
      dplyr::rename(Year = year)
   
    out <- split(temp2, temp2$stock)
  }

  if(display == "bystate"){
    temp3 <-
      temp %>%
      dplyr::select(year, name, stock, print) %>%
      tidyr::spread(stock, print) %>%
      #dplyr::select(year, name, Deshka, 'East Susitna', Talkeetna, Yentna) %>%
      dplyr::rowwise() %>%
      dplyr::mutate_all(nareplace) %>%
      dplyr::arrange(name, year) %>%
      dplyr::rename(Year = year)
    
    out <- split(temp3, temp3$name)
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
                   trib = factor(unlist(trib_id, use.names = FALSE), 
                                 levels = unlist(trib_id, use.names = FALSE),
                                 labels = paste0(unlist(trib_id, use.names = FALSE), " (SD)")),
                   stringsAsFactors = FALSE)
  
  est <- 
    post_dat[["summary"]][grepl("p.S", rownames(post_dat$summary)), c("mean", "sd")] %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    dplyr::mutate(year = as.numeric(gsub("^.*\\[(\\d+).*$", "\\1", rowname)) + yr0,
                  stock = factor(unname(stock_id[gsub("p.S(\\d).*", "\\1", rowname)]), levels = stock_id),
                  tribn = as.numeric(gsub("^.*,(\\d)]$", "\\1", rowname))) %>%
    dplyr::left_join(id, by = c("stock", "tribn")) %>%
    dplyr::mutate(print = paste0(digits(mean), " (", digits(sd), ")")) %>%
    dplyr::select(stock, year, trib, print) 
  
  list <- lapply(stock_id[-1], function(x) est[est$stock == x, ] %>%
                   dplyr::select(-stock) %>%
                   tidyr::spread(trib, print) %>%
                   dplyr::rename(Year = year))
  
  names(list) <- stock_id[-1]
  list
}
