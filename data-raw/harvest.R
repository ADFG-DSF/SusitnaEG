#Note: update import range annually.
library(magrittr)

lut <- data.frame(stock = c("Deshka", rep("East_Susitna", 10), "Talkeetna", rep("Yentna", 5)),
                  trib = c("Deshka", 
                           "Caswell", "Goose", "Kashwitna", "Little Willow", "Montana", "Sheep", "Willow", "Birch", "Rabideux", "Sunshine",
                           "Talkeetna", 
                           "Fish", "Lake", "Peters", "Talachulitna", "Yentna"),
                  stringsAsFactors = FALSE)

Ha_early_raw0 <-
readxl::read_excel(".\\data-raw\\SusitnaEG Ha_pre96.xlsx",
                   range = "Inriver!A5:x23",
                   col_names = c("year", rep("skip", 2), 
                                 "Deshka", "Deshka_up", "skip", 
                                 "Caswell", "Goose", "Kashwitna", "Little Willow", "Montana", "Sheep", "Willow", "Birch", "Rabideux", "Sunshine", "skip",
                                 "Talkeetna", "skip", 
                                 "Fish", "Lake", "Peters", "Talachulitna", "Yentna")) %>%
  dplyr::select(-Deshka_up, -dplyr::starts_with("skip")) %>%
  dplyr::filter(year >= 1979) %>%
  tidyr::gather(trib, H_trib, -year) %>%
  dplyr::left_join(lut, by = "trib")

table(Ha_early_raw0$trib, Ha_early_raw0$stock, useNA = "ifany")
range(apply(table(Ha_early_raw0$trib, Ha_early_raw0$stock, useNA = "ifany"), 1, sum))

Ha_early_raw <-
  Ha_early_raw0 %>%
  dplyr::group_by(year, stock) %>%
  dplyr::summarise(H_stock = sum(H_trib, na.rm = TRUE)) %>%
  tidyr::spread(stock, H_stock) %>%
  dplyr::ungroup()

Ha_late_raw0 <-
  readxl::read_excel(".\\data-raw\\SusitnaEG Ha_post95.xlsx",
                     range = "su_har!A4:H33",
                     col_names = TRUE,
                     na = ".") %>%
  dplyr::select(-Alexander_Cr) %>%
  dplyr::rename(year = Year, Deshka0 = Deshka, Deshka_above0 = Deshka_above)

pct_up <- data.frame(year = Ha_late_raw0$year, pct_up = Ha_late_raw0$Deshka_above0 / 
                     sapply(Ha_late_raw0$Deshka_below + Ha_late_raw0$Deshka_above0, function(x) max(x, 1)))

Ha_late_raw <- 
  dplyr::left_join(Ha_late_raw0, pct_up, by = "year") %>%
  dplyr::mutate(Deshka = ifelse(year <= 1997, Deshka0, Deshka_above0 + Deshka_below + Deshka0),
                Deshka_above = ifelse(year <= 1997, Deshka_above0, Deshka_above0 + Deshka0 * pct_up)) %>%
  dplyr::select(year, Deshka, East_Susitna = East_susitna, Talkeetna, Yentna, Deshka_above)

Hase_late_raw <-
  readxl::read_excel(".\\data-raw\\SusitnaEG Ha_post95.xlsx",
                     range = "ks_se!A4:G33",
                     col_names = TRUE, na = ".") %>%
  dplyr::rename(year = Year, Deshka0 = Deshka, Deshka_above0 = Deshka_above) %>%
  dplyr::left_join(pct_up, by = "year") %>%
  dplyr::mutate(Deshka = ifelse(year <= 1997, Deshka0, sqrt(Deshka_above0^2 + Deshka_below^2 + Deshka0^2)),
                Deshka_above = ifelse(year <= 1997, Deshka_above0, sqrt(pct_up^2 * Deshka0^2 + Deshka_above0^2))) %>%
  dplyr::select(year, Deshka, East_Susitna, Talkeetna, Yentna, Deshka_above)

Ha_cv <-
  rbind(
    data.frame(
      year = Ha_early_raw$year, 
      matrix(
        rep(apply(
              Hase_late_raw[-which(names(Ha_late_raw) == c("year", "Deshka_above"))] / Ha_late_raw[-which(names(Ha_late_raw) == c("year", "Deshka_above"))], 
              MARGIN = 2, 
              quantile, na.rm = TRUE, probs = .75), 
            times = length(Ha_early_raw$year)),
        nrow = length(Ha_early_raw$year), 
        ncol = 4,
        byrow = TRUE) 
      ) %>%
      setNames(names(Ha_late_raw)[-which(names(Ha_late_raw) == "Deshka_above")]),
    data.frame(
      year = Ha_late_raw$year, 
      Hase_late_raw[-which(names(Ha_late_raw) == c("year", "Deshka_above"))] / Ha_late_raw[-which(names(Ha_late_raw) == c("year", "Deshka_above"))]
    )
  ) %>%
  dplyr::mutate_all(.funs = function(x){ifelse(is.na(x), .5, x)}) %>%
  dplyr::rename("East Susitna" = East_Susitna)

Ha0 <-   
  rbind(Ha_early_raw, Ha_late_raw[-which(names(Ha_late_raw) == "Deshka_above")])  %>%
  dplyr::mutate_all(function(x){ifelse(x == 0, 1, round(x))}) %>%
  dplyr::rename("East Susitna" = East_Susitna)

Ha <- 
  list(
    Ha = as.matrix(Ha0[, 2:5]),
    Ha_cv = as.matrix(Ha_cv[, 2:5])
  )

Hd <- 
  dplyr::left_join(Ha0["year"], 
                   Ha_late_raw[which(names(Ha_late_raw) == c("year", "Deshka_above"))], 
                   by = "year")  %>%
  dplyr::rename(H = Deshka_above) %>%
  dplyr::left_join(Hase_late_raw[which(names(Hase_late_raw) == c("year", "Deshka_above"))], by = "year") %>%
  dplyr::mutate(cv = ifelse(is.na(Deshka_above / H), 0.5, Deshka_above / H),
                H = ifelse(is.na(H) | H == 0, 1, H)) %>%
  dplyr::select(-Deshka_above)

Hm <-
  readxl::read_excel(".\\data-raw\\SusitnaEG Hm.xlsx",
                     range = "Marine!ab4:ah52",
                     col_names = TRUE) %>%
  dplyr::mutate_at(.funs = as.integer, .vars = c("year", "SusitnaSR")) %>%
  dplyr::mutate_at(.funs = as.double, .vars = c("CV_SusitnaSR")) %>%
  dplyr::filter(year >= 1979) %>%
  dplyr::select(year, H = SusitnaSR, cv = CV_SusitnaSR)

save(Ha, file=".\\data\\Ha.rda")
save(Hd, file=".\\data\\Hd.rda")
save(Hm, file=".\\data\\Hm.rda")
