age_p <-
readxl::read_excel(".\\SusitnaEG\\data-raw\\Susitna run reconstruction data_Jan102018.xlsx",
                   range = "Deshka brood table!I12:M50",
                   col_names = c("x3", "x4", "x5", "x6", "x7")) %>%
  dplyr::mutate(x67 = x6 + x7)

age <- sapply(age_p[, -which(names(age_p) == "x7")], function(x) as.integer(x * 100)) #effective sample size of 100

#devtools::use_data(age, pkg = ".\\SusitnaEG", overwrite = TRUE)
