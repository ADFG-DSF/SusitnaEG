tribid <- 
  list(
    Deshka = c("Deshka"),
    "East Susitna" = c("Goose", "Kashwitna", "Little Willow", "Montana", "Sheep", "Willow", "Other East Susitna"),
    Talkeetna = c("Clear", "Prairie", "Other Talkeetna"),
    Yentna = c("Cache", "Lake", "Peters", "Talachulitna", "Other Yentna"),
    Other = c("Chulitna", "Indian", "Portage", "Other Other")
  )
stockid <-names(tribid)


tele_dat <- function(year){
  dat <-
    readxl::read_excel(".\\SusitnaEG\\data-raw\\SusitnaEG telemetry_mr.xlsx",
                       sheet = year,
                       range = readxl::cell_limits(ul = c(1, 3), lr = c(NA, 5)),
                       col_names = TRUE) %>%
    dplyr::filter(!is.na(stock)) %>%
    dplyr::mutate(year = as.numeric(year))
  
  dat_trib <-
    dat %>%
    dplyr::group_by(year, stock, trib) %>%
    dplyr::summarise(count = round(sum(Number_Trans)))
  
  stopifnot(min(rowSums(table(dat_trib$trib, dat_trib$stock))) == 1,
            max(rowSums(table(dat_trib$trib, dat_trib$stock))) == 1,
            dat_trib$stock %in% stockid,
            dat_trib$trib[dat_trib$stock == "Deshka"] %in% tribid[["Deshka"]],
            dat_trib$trib[dat_trib$stock == "East Susitna"] %in% tribid[["East Susitna"]],
            dat_trib$trib[dat_trib$stock == "Talkeetna"] %in% tribid[["Talkeetna"]],
            dat_trib$trib[dat_trib$stock == "Yentna"] %in% tribid[["Yentna"]],
            dat_trib$trib[dat_trib$stock == "Other"] %in% tribid[["Other"]])
  
  dat_trib
}

tele12 <- tele_dat("2012")
tele13 <- tele_dat("2013")
abun13 <-
  readxl::read_excel(".\\SusitnaEG\\data-raw\\SusitnaEG telemetry_mr.xlsx",
                     range = "2013Abundance!A1:D7",
                     col_names = TRUE) %>%
  dplyr::group_by(stock) %>%
  dplyr::summarise(N = sum(N),
                   se_N = sqrt(sum(SEN^2))) %>%
  dplyr::mutate(year = 2013) %>%
  dplyr::select(year, stock, N, se_N)
  
all_dat<- function(year){
  dat0 <-
    readxl::read_excel(".\\SusitnaEG\\data-raw\\SusitnaEG telemetry_mr.xlsx",
                       sheet = year,
                       range = readxl::cell_limits(ul = c(1, 2), lr = c(NA, 7)),
                       col_names = TRUE) %>%
    dplyr::filter(!is.na(stock)) %>%
    dplyr::mutate(year = as.numeric(year))
  dat <- dplyr::filter(dat0, !(stock == "Other" & trib == "Other Other" & SPLOC == "Chulitna River"))
  dat_chulitna <- 
    dplyr::filter(dat0, stock == "Other" & trib == "Other Other" & SPLOC == "Chulitna River") %>%
    dplyr::rename(Ntotal = N, se_Ntotal = SEN)
    
  
  chulitna <-
    readxl::read_excel(".\\SusitnaEG\\data-raw\\SusitnaEG telemetry_mr.xlsx",
                       range = "Chulitna tags!A1:D5") %>%
    tidyr::gather(area, n_area, -year, -total) %>%
    dplyr::right_join(dat_chulitna, by = "year") %>%
    dplyr::mutate(p = n_area / total,
                  var_p = p * (1 - p) / (total - 1),
                  N = Ntotal * p,
                  SEN = sqrt(Ntotal^2 * var_p + p^2 * se_Ntotal^2 - var_p * se_Ntotal^2),
                  MOD_TRANS = round(p * MOD_TRANS),
                  trib = ifelse(area == "surveyed", "Chulitna", trib)) %>%
    dplyr::select(year, stock, trib, SPLOC, MOD_TRANS, N , SEN)

  dat_trib <-
    dat %>%
    rbind(chulitna) %>%
    dplyr::group_by(year, stock, trib) %>%
    dplyr::summarise(count = round(sum(MOD_TRANS)),
                     N = sum(N),
                     se_N = sqrt(sum(SEN^2)))
  
  dat_stock <-
    dat_trib %>%
    dplyr::summarise(N = sum(N),
                     se_N = sqrt(sum(se_N^2)))
  
  stopifnot(min(rowSums(table(dat_trib$trib, dat_trib$stock))) == 1,
            max(rowSums(table(dat_trib$trib, dat_trib$stock))) == 1,
            dat_trib$stock %in% stockid,
            dat_trib$trib[dat_trib$stock == "Deshka"] %in% tribid[["Deshka"]],
            dat_trib$trib[dat_trib$stock == "East Susitna"] %in% tribid[["East Susitna"]],
            dat_trib$trib[dat_trib$stock == "Talkeetna"] %in% tribid[["Talkeetna"]],
            dat_trib$trib[dat_trib$stock == "Yentna"] %in% tribid[["Yentna"]],
            dat_trib$trib[dat_trib$stock == "Other"] %in% tribid[["Other"]])
  
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
    dat[dat$stock == stock, c("year", "stock", "trib", "count")] %>%
      dplyr::mutate(trib = factor(trib, 
                                  levels = unlist(lapply(1:5, function(x) c(colnames(as[[x]]), paste0("Other ", names(as[x]))))),
                                  ordered = TRUE)) %>%
      tidyr::spread(trib, count)
  }
  
  dat <- 
    lapply(tele_list, tele, stock = stock) %>%
      do.call(rbind, .) %>%
      dplyr::ungroup() %>%
      dplyr::select(-year, -stock) %>%
      dplyr::mutate_all(dplyr::funs(ifelse(is.na(.), 0, .))) %>%
      as.matrix()
  colnames(dat) <- c(colnames(dat)[-dim(dat)[2]], "Other")

  rbind(matrix(NA, nrow = if(stock == "Yentna") 34 else(33), ncol = dim(dat)[2]),
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
                  stock = factor(stock, stockid, ordered = TRUE)) %>%
    dplyr::select(year, stock, !! param_var) %>%
    tidyr::spread(stock, !! param_var) %>%
    dplyr::select(-year) %>%
    as.matrix()
  
  rbind(matrix(NA, 
               nrow = 34, 
               ncol = dim(mr0)[2]),
        mr0)
}

cv_mr <- abun_matrix(cv)
mr <- list(mr = abun_matrix(N), cv_mr = ifelse(is.na(cv_mr), 0.1, cv_mr))

devtools::use_data(mr, pkg = ".\\SusitnaEG", overwrite = TRUE)
