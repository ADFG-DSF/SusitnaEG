chinBEGs <-
  read.csv(".\\SusitnaEG\\data-raw\\chinBEGs.txt",
           stringsAsFactors = FALSE)

devtools::use_data(chinBEGs, pkg = ".\\SusitnaEG", overwrite = TRUE)
