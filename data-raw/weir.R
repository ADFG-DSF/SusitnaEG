#Note: update import range annually.
library(magrittr)

weir <-
  readxl::read_excel(".\\data-raw\\SusitnaEG weirs.xlsx",
                     range = "Annual counts!A1:D45") %>%
  dplyr::select(-year) %>%
  as.matrix()

save(weir, file=".\\data\\weir.rda")
