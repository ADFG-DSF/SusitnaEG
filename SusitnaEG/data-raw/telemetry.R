S2 <-
  readxl::read_excel(".\\SusitnaEG\\data-raw\\Copy of Susitna run reconstruction by stock.xlsx",
                     range = "Radios!B1:H40") %>%
  as.matrix()

S3 <-
  readxl::read_excel(".\\SusitnaEG\\data-raw\\Copy of Susitna run reconstruction by stock.xlsx",
                     range = "Radios!J1:L40") %>%
  as.matrix()

S4 <-
  readxl::read_excel(".\\SusitnaEG\\data-raw\\Copy of Susitna run reconstruction by stock.xlsx",
                     range = "Radios!N1:R40") %>%
  as.matrix()

S5 <-
  readxl::read_excel(".\\SusitnaEG\\data-raw\\Copy of Susitna run reconstruction by stock.xlsx",
                     range = "Radios!T1:W40") %>%
  as.matrix()

telemetry <- list('East Susitna' = S2, 'N_East Susitna' = rowSums(S2, na.rm = TRUE),
                  'Talkeetna' = S3, 'N_Talkeetna' = rowSums(S3, na.rm = TRUE),
                  'Yentna' = S4, 'N_Yentna' = rowSums(S4, na.rm = TRUE),
                  'Other' = S5, 'N_Other' = rowSums(S5, na.rm = TRUE))

devtools::use_data(telemetry, pkg = ".\\SusitnaEG", overwrite = TRUE)
