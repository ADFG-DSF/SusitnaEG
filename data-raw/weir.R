weir <-
  readxl::read_excel(".\\data-raw\\SusitnaEG weirs.xlsx",
                     range = "Annual counts!A1:D44") %>%
  dplyr::select(-year) %>%
  as.matrix()

save(weir, file=".\\data\\weir.rda")
