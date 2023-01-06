chinBEGs <-
  read.csv(".\\data-raw\\chinBEGs.txt",
           stringsAsFactors = FALSE)

save(chinBEGs, file=".\\data\\chinBEGs.rda")
