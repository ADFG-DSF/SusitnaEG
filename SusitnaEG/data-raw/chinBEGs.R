chinBEGs <-
  read.csv(".\\SusitnaEG\\data-raw\\chinBEGs.txt",
           stringsAsFactors = FALSE) %>%
  dplyr::filter(Stock != "Deshka")

devtools::use_data(chinBEGs, pkg = ".\\SusitnaEG", overwrite = TRUE)
