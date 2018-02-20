rawage_deshka <-
  readxl::read_excel(".\\SusitnaEG\\data-raw\\Susitna run reconstruction data_Jan102018.xlsx",
                   range = "Deshka brood table!I12:M50",
                   col_names = c("x3", "x4", "x5", "x6", "x7"))

age_deshka <- 
  sapply(rawage_deshka, function(x) as.integer(x * 100)) %>% #effective sample size of 100
  as.data.frame() %>%
  dplyr::mutate(year = as.character(1979:2017),
                location = ifelse(year %in% as.character(1979:1985), "Deshka creel", "Deshka weir")) %>%
  dplyr::rename(x78 = x7)

age_alex <-
  readxl::read_excel(".\\SusitnaEG\\data-raw\\Copy of Alexander age comp.xls",
                     range = "Alexander age comp!A11:K21",
                     col_types = c("text", rep("skip", 2), rep("numeric", 6), "skip", "numeric"),
                     col_names = c("year", "p3", "p4", "p5", "p6", "p7", "p6_2", "n")) %>%
  dplyr::mutate_all(function(x) ifelse(is.na(x), 0, x)) %>%
  dplyr::mutate(p6 = p6 + p6_2,
                x3 = as.integer(p3 / 100 * n),
                x4 = as.integer(p4 / 100  * n),
                x5 = as.integer(p5 / 100  * n),
                x6 = as.integer(p6 / 100  * n),
                x78 = as.integer(p7 / 100  * n),
                location = "Alexander creel") %>%
  dplyr::select(-dplyr::starts_with("p")) %>%
  dplyr::filter(n != 0)

rawage_cfprelim <-
  readxl::read_excel(".\\SusitnaEG\\data-raw\\comm fish data\\Copy of KING_CHUM_COHO_DATA.xlsx",
                     range = "King data!A2:P15650",
                     col_names = c("daycode", "year", "location_raw", "spp", "mefl", "sex", "age", "age_error"),
                     col_types = c(rep("text", 2), rep("skip", 3), "text", rep("skip", 4), "text", "numeric", "skip", rep("text", 3)),
                     skip = 1, 
                     na = "-999") 

lapply(rawage_cfprelim, table, useNA = "ifany")
rawage_cfprelim[is.na(rawage_cfprelim$daycode), ] %>% print(n = 500) # keep
rawage_cfprelim[is.na(rawage_cfprelim$year), ] %>% print(n = 100) #drop
table(rawage_cfprelim$location_raw, rawage_cfprelim$year)
rawage_cfprelim[is.na(rawage_cfprelim$spp), ] %>% print(n = 100) #drop
rawage_cfprelim[(rawage_cfprelim$mefl <= 200) | (rawage_cfprelim$mefl >= 1600) | is.na(rawage_cfprelim$mefl), ] %>% print(n = 100) #make NA
rawage_cfprelim[!(rawage_cfprelim$sex %in% c( "1", "2")), ] %>% print(n = 100) #make NA
rawage_cfprelim[!(rawage_cfprelim$age %in% c( "11", "12", "13", "14", "15", "16")) & !is.na(rawage_cfprelim$age), ] %>% print(n = 600) #keep 2., otherwise make NA
rawage_cfprelim[(rawage_cfprelim$age_error %in% c("12", "32")), ] %>% print(n = 100) #errors but ingnore
table(rawage_cfprelim$age, rawage_cfprelim$age_error)

age_cfprelim <- 
  rawage_cfprelim %>%
  dplyr::filter(!is.na(year),
                !is.na(spp)) %>%
  dplyr::mutate(location = ifelse(location_raw == "Susuitna river- Curry", "Susitna River-Curry", location_raw),
                location = ifelse(location == "Yentna River escapement", "Yentna River Escapement", location),
                mefl = ifelse((mefl <= 200) | (mefl >= 1600), NA, mefl),
                sex = ifelse(sex %in% c("1", "2"), sex, NA),
                age = ifelse(age %in% c( "11", "12", "13", "14", "15", "16", "21", "22", "23", "24"), age, NA)) %>%
  dplyr::filter(!is.na(age),
                grepl("Susitna|Yentna", location_raw)) %>%
  dplyr::mutate(age_f = forcats::fct_collapse(factor(age),
                                              "x3" = "11", "x4" = c("12", "21"), "x5" = c("13", "22"), "x6" = c("14", "23"), "x78" = c("15", "24", "16")))

lapply(age_cfprelim, table, useNA = "ifany")
table(age_cfprelim$location, age_cfprelim$year)

rawage_cf <-
  readr::read_fwf(".\\SusitnaEG\\data-raw\\comm fish data\\MASTER_ASL_DATABASE_ARCHIVE_1967-2017.txt",
                  col_positions = readr::fwf_positions(c(26, 31, 67, 146, 160), c(29, 34, 77, 148, 163), c("daycode", "year", "stat_code", "spp", "age")),
                  col_types = "ccccc") %>%
  dplyr::filter(spp == "410",
                stat_code %in% c("24741100600", "24741100803", "24741100804", "24741100805", "24741100901", "24741101802", "24741103804")) %>%
  dplyr::mutate_all(function(x) ifelse(x == "-999", NA, x))

lapply(rawage_cf, table, useNA = "ifany")
table(rawage_cf$stat_code, rawage_cf$year)
rawage_cf[!(rawage_cf$age %in% c( "11", "12", "13", "14", "15", "16")) & !is.na(rawage_cf$age), ] %>% print(n = 600) #keep 2., otherwise make NA

age_cf <- 
  rawage_cf %>%
  dplyr::mutate(age = ifelse(age %in% c( "11", "12", "13", "14", "15", "16", "21", "22", "23", "24"), age, NA)) %>%
  dplyr::filter(!is.na(age)) %>%
  dplyr::mutate(age_f = forcats::fct_collapse(factor(age),
                                              "x3" = "11", "x4" = c("12", "21"), "x5" = c("13", "22"), "x6" = c("14", "23"), "x78" = c("15", "24", "16")),
                location = forcats::fct_collapse(factor(stat_code),
                                                 "Susitna River - Flathorn" = "24741100600",
                                                 "Susitna River Escapement" = c("24741100901", "24741103804"),
                                                 "Yentna River Escapement" = "24741101802"))

lapply(age_cf, table, useNA = "ifany")
table(age_cf$location, age_cf$year)

age_comm <- rbind(age_cfprelim[, c("daycode", "year", "age_f", "location")], age_cf[, c("daycode", "year", "age_f", "location")])

id <-unique((age_comm[, c("year", "location")]))
xage <- 
  t(mapply(function(x, y){ 
  tab <- table(age_comm$age_f[age_comm$location == x & age_comm$year == y])},
  x = id[[2]],
  y = id[[1]],
  USE.NAMES = FALSE)) %>%
  as.data.frame() %>%
  dplyr::mutate(year = id[[1]],
                location = id[[2]]) %>%
  dplyr::select(year, location, dplyr::everything()) %>%
  dplyr::bind_rows(age_deshka, age_alex) %>%
  dplyr::mutate(n = x3 + x4 + x5 + x6 + x78,
                p3 = round(x3 / n, 2),
                p4 = round(x4 / n, 2),
                p5 = round(x5 / n, 2),
                p6 = round(x6 / n, 2),
                p78 = round(x78 / n, 2)) %>%
  dplyr::arrange(year, location) 

library(ggplot2)
xage %>% 
  dplyr::select(year, location, dplyr::starts_with("p")) %>%
  tidyr::gather(age, prop, -year, - location) %>%
  ggplot(aes(x = year, y = prop, color = location)) + geom_point() + facet_grid(age ~ ., scales = "free_y")

age <- xage %>% dplyr::select(-dplyr::starts_with("p"))
devtools::use_data(age, pkg = ".\\SusitnaEG", overwrite = TRUE)
