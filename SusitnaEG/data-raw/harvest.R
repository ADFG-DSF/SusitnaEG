lut <- data.frame(stock = c("Deshka_down0", "Deshka_up0", rep("East_Susitna0", 10), "Talkeetna0", rep("Yentna0", 5), "All"),
                  trib = c("Deshka_down", "Deshka_up", 
                           "Caswell", "Goose", "Kashwitna", "Little Willow", "Montana", "Sheep", "Willow", "Birch", "Rabideux", "Sunshine",
                           "Talkeetna", 
                           "Fish", "Lake", "Peters", "Talachulitna", "Yentna", "Total_above"),
                  stringsAsFactors = FALSE)

Ha_raw <-
readxl::read_excel(".\\SusitnaEG\\data-raw\\SusitnaEG harvest_11122018.xlsx",
                   range = "Inriver!A4:z45",
                   col_names = TRUE) %>%
  dplyr::select(-Alexander, -dplyr::starts_with("X")) %>%
  dplyr::filter(year >= 1979) %>%
  tidyr::gather(trib, H_trib, -year) %>%
  dplyr::left_join(lut, by = "trib")

table(Ha_raw$trib, Ha_raw$stock, useNA = "ifany")
range(apply(table(Ha_raw$trib, Ha_raw$stock, useNA = "ifany"), 1, sum))

temp <-
  Ha_raw %>%
  dplyr::group_by(year, stock) %>%
  dplyr::summarise(H_stock = sum(H_trib, na.rm = TRUE)) %>%
  tidyr::spread(stock, H_stock) %>%
  dplyr::mutate(sum = sum(Deshka_down0, Deshka_up0, East_Susitna0, Talkeetna0, Yentna0),
                All = ifelse(All > sum, All, sum),
                nostock = All - sum,
                Deshka = Deshka_down0 + nostock * Deshka_down0 / All,
                Deshka_up = Deshka_up0 + nostock * Deshka_up0 / All,
                East_Susitna = East_Susitna0 + nostock * East_Susitna0 / All,
                Talkeetna = Talkeetna0 + nostock * Talkeetna0 / All,
                Yentna = Yentna0 + nostock * Yentna0 / All) %>%
  dplyr::ungroup() %>%
  dplyr::mutate_all(function(x){ifelse(x == 0, 1, round(x))})

Ha <- temp[, c("year", "Deshka", "East_Susitna", "Talkeetna", "Yentna")]
devtools::use_data(Ha, pkg = ".\\SusitnaEG", overwrite = TRUE)
Hd <- temp[, c("year", "Deshka_up")]
devtools::use_data(Hd, pkg = ".\\SusitnaEG", overwrite = TRUE)

Hm <-
  readxl::read_excel(".\\SusitnaEG\\data-raw\\SusitnaEG harvest.xlsx",
                     range = "Marine!z4:aa45",
                     col_names = TRUE) %>%
  dplyr::mutate_all(as.integer) %>%
  dplyr::filter(year >=1979)

devtools::use_data(Hm, pkg = ".\\SusitnaEG", overwrite = TRUE)
