weir <-
  readxl::read_excel(".\\SusitnaEG\\data-raw\\SusitnaEG weirs.xlsx",
                     range = "Annual counts!A1:D43") %>%
  dplyr::select(-year) %>%
  as.matrix()

save(weir, file=".\\SusitnaEG\\data\\weir.rda")
