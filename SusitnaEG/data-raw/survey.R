lut <- data.frame(stock = c("Deshka", rep("East_Susitna", 7), rep("Talkeetna", 2), rep("Yentna", 4), rep("Other", 3)),
                  trib = c("Deshka", "Deception", "Goose", "Kashwitna", "Little Willow", "Montana", "Sheep", "Willow", "Clear", "Prairie", 
                           "Cache", "Lake", "Peters", "Talachulitna", "Chulitna", "Indian", "Portage"),
                  stringsAsFactors = FALSE)

survey_raw <-
  readxl::read_excel(".\\SusitnaEG\\data-raw\\SusitnaEG survey.xlsx",
                     range = "Single aerial survey counts!A5:X45",
                     col_names = c("year", "Alexander", "skip", 
                                   "Deshka", "skip",
                                   "Deception", "Goose", "Kashwitna", "Little Willow", "Montana", "Sheep", "Willow", "skip",
                                   "Clear", "Prairie", "skip", 
                                   "Cache", "Lake", "Peters", "Talachulitna", "skip",
                                   "Chulitna", "Indian", "Portage"), 
                     col_types = c(rep("numeric", 2), "skip",
                                   "numeric", "skip",
                                   rep("numeric", 7), "skip",
                                   rep("numeric", 2), "skip",
                                   rep("numeric", 4), "skip",
                                   rep("numeric", 3))) %>%
  dplyr::mutate(Willow = Willow + Deception) %>%
  dplyr::select(-Alexander, -Deception, -dplyr::starts_with("skip")) %>%
  tidyr::gather(trib, count, -year) %>%
  dplyr::left_join(lut, by = "trib")

make_list <- function(stock){
  survey_raw[survey_raw$stock == stock, ] %>%
    dplyr::select(-stock) %>%
    tidyr::spread(trib, count) %>%
    dplyr::select(-year) %>%
    as.matrix()
}

as <- list(Deshka = make_list("Deshka"),
           'East Susitna' = make_list("East_Susitna"),
           Talkeetna = make_list("Talkeetna"),
           Yentna = make_list("Yentna"))

save(as, file=".\\SusitnaEG\\data\\as.rda")

