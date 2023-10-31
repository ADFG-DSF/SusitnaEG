#Note: update NA range for telemetry, mr and mr2018 data annually.
library(magrittr)


#weighted tag numbers
tribid <- 
  list(
    Deshka = c("Deshka"),
    "East Susitna" = c("Goose", "Kashwitna", "Little Willow", "Montana", "Sheep", "Willow", "Other Eastside Susitna"),
    Talkeetna = c("Clear", "Prairie", "Other Talkeetna River"),
    Yentna = c("Cache", "Lake", "Peters", "Talachulitna", "Other Yentna River")
  )
stockid <-names(tribid)

tele_dat <- function(year){
  dat <-
    readxl::read_excel(".\\data-raw\\SusitnaEG telemetry_mr_JKC_11262018.xlsx",
                       sheet = year,
                       range = readxl::cell_limits(ul = c(2, 3), lr = c(NA, 5)),
                       col_names = c("trans", "stock", "trib")) %>%
    dplyr::filter(!is.na(stock) & stock != "Other") %>%
    dplyr::mutate(year = as.numeric(year),
                  trib = ifelse(trib == "Other East Susitna", "Other Eastside Susitna", 
                                ifelse(trib == "Other Talkeetna", "Other Talkeetna River",
                                       ifelse(trib == "Other Yentna", "Other Yentna River", trib))))
  
  dat_trib <-
    dat %>%
    dplyr::group_by(year, stock, trib) %>%
    dplyr::summarise(count = round(sum(trans)))
  
  stopifnot(min(rowSums(table(dat_trib$trib, dat_trib$stock))) == 1,
            max(rowSums(table(dat_trib$trib, dat_trib$stock))) == 1,
            dat_trib$stock %in% stockid,
            dat_trib$trib[dat_trib$stock == "Deshka"] %in% tribid[["Deshka"]],
            dat_trib$trib[dat_trib$stock == "East Susitna"] %in% tribid[["East Susitna"]],
            dat_trib$trib[dat_trib$stock == "Talkeetna"] %in% tribid[["Talkeetna"]],
            dat_trib$trib[dat_trib$stock == "Yentna"] %in% tribid[["Yentna"]])
  
  dat_trib
}

tele12 <- tele_dat("2012")
tele13 <- tele_dat("2013")
tele14 <- tele_dat("2014")
tele15 <- tele_dat("2015")
tele16 <- tele_dat("2016")
tele17 <- tele_dat("2017")

tele_list <- list(tele12, tele13, tele14, tele15, tele16, tele17)
tele_matrix <- function(stock){
  tele <- function(dat, stock){
    dat[dat$stock == stock, c("year", "stock", "trib", "count")] %>%
      dplyr::mutate(trib = factor(trib, 
                                  levels = unlist(tribid, use.names = FALSE),
                                  ordered = TRUE)) %>%
      tidyr::spread(trib, count)
  }
  
  out <- 
    lapply(tele_list, tele, stock = stock) %>%
      do.call(rbind, .) %>%
      dplyr::ungroup() %>%
      dplyr::select(-year, -stock) %>%
      dplyr::mutate_all(list(~ ifelse(is.na(.), 0, .))) %>%
      as.matrix()

  rbind(matrix(NA, nrow = if(stock == "Yentna") 35 else(34), ncol = dim(out)[2]), out) 
}

tele_east <- tele_matrix("East Susitna") %>% rbind(matrix(NA, nrow = 5, ncol = dim(.)[2])) ####increment NA # here
tele_tal <- tele_matrix("Talkeetna")%>% rbind(matrix(NA, nrow = 5, ncol = dim(.)[2])) ####increment NA # here
tele_yent <- tele_matrix("Yentna") %>% rbind(matrix(NA, nrow = 5, ncol = dim(.)[2])) ####increment NA # here

telemetry <- list('East Susitna' = tele_east, 'N_East Susitna' = rowSums(tele_east, na.rm = TRUE),
                  'Talkeetna' = tele_tal, 'N_Talkeetna' = rowSums(tele_tal, na.rm = TRUE),
                  'Yentna' = tele_yent, 'N_Yentna' = rowSums(tele_yent, na.rm = TRUE))


save(telemetry, file=".\\data\\telemetry.rda")

#mark recapture estimates
lut <- c("C" = "Deshka", "E+B" = "East Susitna", "F" = "Talkeetna", "1to5" = "Yentna")
temp <-
  readxl::read_excel(".\\data-raw\\SusitnaEG telemetry_mr_JKC_11262018.xlsx",
                   sheet = "2012-2017_AB_BY_SPGRP",
                   range = readxl::cell_limits(ul = c(3, NA), lr = c(NA, 10)),
                   col_names = TRUE) %>%
  dplyr::filter(ALT_SPGRP %in% c("C", "E+B", "F", "1to5")) %>%
  dplyr::select(year = YEAR, group = ALT_SPGRP, N = SPGRP_AB, seN = "SE(AB)") %>%
  tidyr::fill(year) %>%
  dplyr::mutate(stock = lut[group],
                cv = seN / N)

#2019 estimate
temp2 <-
  readxl::read_excel(".\\data-raw\\Copy of MASTER_SUSITNA_2019_CHINOOK_ABUNDANCE_TELEMETRY_9_15_20_FOR_NICK.xlsx",
                     sheet = "MAIN_DIST_SPGRP_2019",
                     range = "B1:R4",
                     col_names = TRUE) %>%
  dplyr::mutate(SPGRP = ifelse(SPGRP == "E", "E+B", SPGRP)) %>%
  dplyr::select(group = SPGRP, N = Nsmlg, seN = SENsmlg) %>%
  dplyr::mutate(year = 2019,
                stock = lut[group],
                cv = seN / N)

#2020 estimate
temp3 <-
  readxl::read_excel(".\\data-raw\\MASTER_SUSITNA_2020_CHINOOK_ABUNDANCE_TELEMETRY_10_1_20_FOR_NICK.xlsx",
                     sheet = "MAIN_DIST_SPGRP_2020",
                     range = "B1:R4",
                     col_names = TRUE) %>%
  dplyr::mutate(SPGRP = ifelse(SPGRP == "E", "E+B", SPGRP)) %>%
  dplyr::select(group = SPGRP, N = Nsmlg, seN = SENsmlg) %>%
  dplyr::mutate(year = 2020,
                stock = lut[group],
                cv = seN / N)

#2021 estimate
temp4 <-
  readxl::read_excel(".\\data-raw\\2021 Susita MR Abundance by stock.xlsx",
                     sheet = "Sheet 1",
                     range = "A1:G6",
                     col_names = TRUE) %>%
  dplyr::mutate(SPGRP = ifelse(SPGRP == "E", "E+B", SPGRP)) %>%
  dplyr::filter(SPGRP %in% c("C", "E+B", "F")) %>%
  dplyr::select(group = SPGRP, N = Nsp, seN = SEN) %>%
  dplyr::mutate(year = 2021,
                stock = lut[group],
                cv = seN / N)

fill <- #Increment NAs: Add new year here if no MR
  data.frame(year = rep(c(1979:2012, 2018, 2022, 2023), times = 4), 
             stock = rep(c("Deshka", "East Susitna", "Talkeetna", "Yentna"), each = 37),
             N = NA, 
             cv = 0.1, group = NA, seN = NA)

mr2018 <- data.frame(year = 1979:2023) #increment year range here
mr2018$mr_det <- ifelse(mr2018$year == "2018", 30605, NA) #det = Deshka, Eastside, Talkeetna
mr2018$tau.logmr_det <- ifelse(mr2018$year == "2018", 1 / log((4376 / 30605)^2 + 1), 0.1)

NAreplace <- function(x) ifelse(is.na(x), 0.1, x)
mr <- 
  list(mr = rbind(temp, temp2, temp3, temp4, fill) %>%
        dplyr::select(stock, year, N) %>%
        tidyr::spread(stock, N) %>%
        dplyr::select(-year) %>%
        as.matrix(),
      cv_mr = rbind(temp, temp2, temp3, temp4, fill) %>%
        dplyr::select(stock, year, cv) %>%
        tidyr::spread(stock, cv) %>%
        dplyr::select(-year) %>%
        dplyr::mutate_all(list(NAreplace)) %>% 
        as.matrix(),
      mr_det = mr2018$mr_det,
      tau.logmr_det = mr2018$tau.logmr_det)

save(mr, file=".\\data\\mr.rda")
