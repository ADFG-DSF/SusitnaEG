chinBEGs <-
  read.csv(".\\SusitnaEG\\data-raw\\chinBEGs.txt",
           stringsAsFactors = FALSE) %>%
  dplyr::filter(Stock != "Deshka")

save(chinBEGs, file=".\\data\\chinBEGs.rda")
