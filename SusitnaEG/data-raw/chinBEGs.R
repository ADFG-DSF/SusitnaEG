dat_chinBEGs <-
  read.csv(".\\SusitnaEG\\data-raw\\chinBEGs.txt",
           stringsAsFactors = FALSE)

devtools::use_data(dat_chinBEGs, pkg = ".\\SusitnaEG", overwrite = TRUE)
