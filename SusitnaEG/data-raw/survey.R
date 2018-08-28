lut <- data.frame(stock = c("Deshka", rep("East_Susitna", 7), rep("Talkeetna", 2), rep("Yentna", 4), rep("Other", 3)),
                  trib = c("Deshka", "Deception", "Goose", "Kashwitna", "Little Willow", "Montana", "Sheep", "Willow", "Clear", "Prairie", 
                           "Cache", "Lake", "Peters", "Talachulitna", "Chulitna", "Indian", "Portage"),
                  stringsAsFactors = FALSE)

survey_raw <-
  readxl::read_excel(".\\SusitnaEG\\data-raw\\SusitnaEG survey.xlsx",
                     range = "Single aerial survey counts!A4:X43",
                     col_names = TRUE) %>%
  dplyr::mutate(Willow = Willow + Deception) %>%
  dplyr::select(-Alexander, -Deception, -dplyr::starts_with("X")) %>%
  tidyr::gather(trib, count, -year) %>%
  dplyr::left_join(lut, by = "trib")

make_list <- function(stock){
  survey_raw[survey_raw$stock == stock, ] %>%
    dplyr::select(-stock) %>%
    tidyr::spread(trib, count) %>%
    dplyr::select(-year) %>%
    as.matrix()
}

as2 <- list(Deshka = make_list("Deshka"),
           'East Susitna' = make_list("East_Susitna"),
           Talkeetna = make_list("Talkeetna"),
           Yentna = make_list("Yentna"),
           Other = make_list("Other"))

devtools::use_data(as, pkg = ".\\SusitnaEG", overwrite = TRUE)
