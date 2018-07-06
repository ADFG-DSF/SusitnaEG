mr <-
  readxl::read_excel(".\\SusitnaEG\\data-raw\\Copy of Susitna run reconstruction by stock.xlsx",
                     range = "MR!B1:F40") %>%
  as.matrix()

devtools::use_data(mr, pkg = ".\\SusitnaEG", overwrite = TRUE)
