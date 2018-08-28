weir <-
  readxl::read_excel(".\\SusitnaEG\\data-raw\\SusitnaEG weirs.xlsx",
                     range = "Annual counts!A1:D40") %>%
  dplyr::select(-year) %>%
  as.matrix()

devtools::use_data(weir, pkg = ".\\SusitnaEG", overwrite = TRUE)

