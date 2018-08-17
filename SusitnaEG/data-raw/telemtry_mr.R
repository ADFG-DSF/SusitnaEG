library(SusitnaEG)
get_ids()
tele_dat <- function(year){
  dat <-
    readxl::read_excel(".\\SusitnaEG\\data-raw\\Copy of DATA_JKC_7_27_18 v2.xlsx",
                       sheet = year,
                       range = readxl::cell_limits(ul = c(2, 2), lr = c(NA, 5)),
                       col_names = c("stock", "trib", "name", "radios")) %>%
    dplyr::filter(!is.na(stock)) %>%
    dplyr::mutate(year = as.numeric(year))
  
  dat_trib <-
    dat %>%
    dplyr::group_by(year, stock, trib) %>%
    dplyr::summarise(radio = round(sum(radios)))
  
  stopifnot(min(rowSums(table(dat_trib$trib, dat_trib$stock))) == 1,
            max(rowSums(table(dat_trib$trib, dat_trib$stock))) == 1,
            dat_trib$stock %in% stock_id,
            dat_trib$trib[dat_trib$stock == "Deshka"] %in% trib_id[["Deshka"]],
            dat_trib$trib[dat_trib$stock == "East Susitna"] %in% trib_id[["East Susitna"]],
            dat_trib$trib[dat_trib$stock == "Talkeetna"] %in% trib_id[["Talkeetna"]],
            dat_trib$trib[dat_trib$stock == "Yentna"] %in% trib_id[["Yentna"]],
            dat_trib$trib[dat_trib$stock == "Other"] %in% trib_id[["Other"]])
  
  dat_trib
}

tele12 <- tele_dat("2012")
tele13 <- tele_dat("2013")
abun13 <-
  readxl::read_excel(".\\SusitnaEG\\data-raw\\Copy of DATA_JKC_7_27_18 v2.xlsx",
                     range = "2013Abundance!A1:D7",
                     col_names = TRUE) %>%
  dplyr::group_by(stock) %>%
  dplyr::summarise(N = sum(N),
                   se_N = sqrt(sum(se^2))) %>%
  dplyr::mutate(year = 2013) %>%
  dplyr::select(year, stock, N, se_N)
  
all_dat<- function(year){
  dat0 <-
    readxl::read_excel(".\\SusitnaEG\\data-raw\\Copy of DATA_JKC_7_27_18 v2.xlsx",
                       sheet = year,
                       range = readxl::cell_limits(ul = c(2, 2), lr = c(NA, 7)),
                       col_names = c("stock", "trib", "name", "radios", "N", "se_N")) %>%
    dplyr::filter(!is.na(stock)) %>%
    dplyr::mutate(year = as.numeric(year))
  dat <- dplyr::filter(dat0, !(stock == "Other" & trib == "Other Other" & name == "Chulitna River"))
  dat_chulitna <- 
    dplyr::filter(dat0, stock == "Other" & trib == "Other Other" & name == "Chulitna River") %>%
    dplyr::rename(Ntotal = N, se_Ntotal = se_N)
    
  
  chulitna <-
    readxl::read_excel(".\\SusitnaEG\\data-raw\\Copy of DATA_JKC_7_27_18 v2.xlsx",
                       range = "Chulitna tags!A1:D5") %>%
    tidyr::gather(area, n_area, -year, -total) %>%
    dplyr::right_join(dat_chulitna, by = "year") %>%
    dplyr::mutate(n_area = ifelse(year == 2017, ifelse(area == "not_surveyed", 
                                                                  total * .45, 
                                                                  total * .55), 
                                  n_area), # TEMP till John gets back with me
                  p = n_area / total,
                  var_p = p * (1 - p) / (total - 1),
                  N = Ntotal * p,
                  se_N = sqrt(Ntotal^2 * var_p + p^2 * se_Ntotal^2 - var_p * se_Ntotal^2),
                  radios = round(p * radios),
                  trib = ifelse(area == "surveyed", "Chulitna", trib)) %>%
    dplyr::select(year, stock, trib, name, radios, N , se_N)

  dat_trib <-
    dat %>%
    rbind(chulitna) %>%
    dplyr::group_by(year, stock, trib) %>%
    dplyr::summarise(radio = round(sum(radios)),
                     N = sum(N),
                     se_N = sqrt(sum(se_N^2)))
  
  dat_stock <-
    dat_trib %>%
    dplyr::summarise(N = sum(N),
                     se_N = sqrt(sum(se_N^2)))
  
  stopifnot(min(rowSums(table(dat_trib$trib, dat_trib$stock))) == 1,
            max(rowSums(table(dat_trib$trib, dat_trib$stock))) == 1,
            dat_trib$stock %in% stock_id,
            dat_trib$trib[dat_trib$stock == "Deshka"] %in% trib_id[["Deshka"]],
            dat_trib$trib[dat_trib$stock == "East Susitna"] %in% trib_id[["East Susitna"]],
            dat_trib$trib[dat_trib$stock == "Talkeetna"] %in% trib_id[["Talkeetna"]],
            dat_trib$trib[dat_trib$stock == "Yentna"] %in% trib_id[["Yentna"]],
            dat_trib$trib[dat_trib$stock == "Other"] %in% trib_id[["Other"]])
  
  list(dat_trib = dat_trib, 
       dat_stock = dat_stock)
}

dat14 <- all_dat("2014")
dat15 <- all_dat("2015")
dat16 <- all_dat("2016")
dat17 <- all_dat("2017")

tele_list <- list(tele12, tele13, dat14[[1]], dat15[[1]], dat16[[1]], dat17[[1]])
tele_matrix <- function(stock){
  tele <- function(dat, stock){
    dat[dat$stock == stock, c("year", "stock", "trib", "radio")] %>%
      dplyr::mutate(trib = factor(trib, 
                                  levels = unlist(lapply(1:5, function(x) c(colnames(as[[x]]), paste0("Other ", names(as[x]))))),
                                  ordered = TRUE)) %>%
      tidyr::spread(trib, "radio")
  }
  
  dat <- 
    lapply(tele_list, tele, stock = stock) %>%
      do.call(rbind, .) %>%
      dplyr::ungroup() %>%
      dplyr::select(-year, -stock) %>%
      dplyr::mutate_all(dplyr::funs(ifelse(is.na(.), 0, .))) %>%
      as.matrix()
  colnames(dat) <- c(colnames(dat)[-dim(dat)[2]], "Other")

  rbind(matrix(NA, nrow = length(year_id) - dim(dat)[1], ncol = dim(dat)[2]),
        dat)
}

tele_east <- tele_matrix("East Susitna")
tele_tal <- tele_matrix("Talkeetna")
tele_yent <- tele_matrix("Yentna")
tele_other <- tele_matrix("Other")

telemetry <- list('East Susitna' = tele_east, 'N_East Susitna' = rowSums(tele_east, na.rm = TRUE),
                  'Talkeetna' = tele_tal, 'N_Talkeetna' = rowSums(tele_tal, na.rm = TRUE),
                  'Yentna' = tele_yent, 'N_Yentna' = rowSums(tele_yent, na.rm = TRUE),
                  'Other' = tele_other, 'N_Other' = rowSums(tele_other, na.rm = TRUE))

devtools::use_data(telemetry, pkg = ".\\SusitnaEG", overwrite = TRUE)



abun_list <- list(abun13, dat14[[2]], dat15[[2]], dat16[[2]], dat17[[2]])
abun_matrix <- function(param){
  param_var <- dplyr::enquo(param)
  mr0 <- 
    lapply(abun_list, dplyr::ungroup) %>%
    do.call(rbind, .) %>%
    dplyr::mutate(cv = se_N / N,
                  stock = factor(stock, stock_id, ordered = TRUE)) %>%
    dplyr::select(year, stock, !! param_var) %>%
    tidyr::spread(stock, !! param_var) %>%
    dplyr::select(-year) %>%
    as.matrix()
  
  rbind(matrix(NA, nrow = length(year_id) - dim(mr0)[1], ncol = dim(mr0)[2]),
        mr0)
}

mr <- list(mr = abun_matrix(N), cv_mr = abun_matrix(cv))
devtools::use_data(mr, pkg = ".\\SusitnaEG", overwrite = TRUE)
