lut <- data.frame(stock = c("Deshka0", rep("East_Susitna0", 7), "Talkeetna0", rep("Yentna0", 5), rep("Other0", 3), "All"),
                  trib = c("Deshka", "Caswell", "Goose", "Kashwitna", "Little Willow", "Montana", "Sheep", "Willow", "Talkeetna", 
                           "Fish", "Lake", "Peters", "Talachulitna", "Yentna", "Birch", "Rabideux", "Sunshine", "Total_above"),
                  stringsAsFactors = FALSE)

Ha_raw <-
readxl::read_excel(".\\SusitnaEG\\data-raw\\SusitnaEG harvest.xlsx",
                   range = "Inriver!A4:z43",
                   col_names = TRUE) %>%
  dplyr::select(-Alexander, -dplyr::starts_with("X")) %>%
  dplyr::filter(year >= 1979) %>%
  tidyr::gather(trib, H_trib, -year) %>%
  dplyr::left_join(lut, by = "trib")

table(Ha_raw$trib, Ha_raw$stock, useNA = "ifany")
range(apply(table(Ha_raw$trib, Ha_raw$stock, useNA = "ifany"), 1, sum))

Ha <-
  Ha_raw %>%
  dplyr::group_by(year, stock) %>%
  dplyr::summarise(H_stock = sum(H_trib, na.rm = TRUE)) %>%
  tidyr::spread(stock, H_stock) %>%
  dplyr::mutate(sum = sum(Deshka0, East_Susitna0, Talkeetna0, Yentna0, Other0),
                nostock = All - sum,
                Deshka = Deshka0 + nostock * Deshka0 / All,
                East_Susitna = East_Susitna0 + nostock * East_Susitna0 / All,
                Talkeetna = Talkeetna0 + nostock * Talkeetna0 / All,
                Yentna = Yentna0 + nostock * Yentna0 / All,
                Other = Other0 + nostock * Other0 / All) %>%
  dplyr::ungroup() %>%
  dplyr::select(year, Deshka, East_Susitna, Talkeetna, Yentna, Other) %>%
  dplyr::mutate_all(function(x){ifelse(x == 0, 1, round(x))})

devtools::use_data(Ha, pkg = ".\\SusitnaEG", overwrite = TRUE)

Hm <-
  readxl::read_excel(".\\SusitnaEG\\data-raw\\SusitnaEG harvest.xlsx",
                     range = "Marine!z4:aa45",
                     col_names = TRUE) %>%
  dplyr::mutate_all(as.integer) %>%
  dplyr::filter(year >=1979)

devtools::use_data(Hm, pkg = ".\\SusitnaEG", overwrite = TRUE)
