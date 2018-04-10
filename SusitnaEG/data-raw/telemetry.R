telemetry <-
  readxl::read_excel(".\\SusitnaEG\\data-raw\\Copy of SusitnaChinookSSMData_TEL_COUNTS_FIXED_2_23_18.xlsx",
                     range = "Sheet1!AA4:AK42",
                     col_names = LETTERS[2:14][-c(3, 8)]) %>%
  dplyr::mutate_all(function(x) ifelse(is.na(x), 0, round(x)))

devtools::use_data(telemetry, pkg = ".\\SusitnaEG", overwrite = TRUE)
