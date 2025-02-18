#Note: update import range annually.
sonar0 <-
  readxl::read_excel(".\\data-raw\\SusitnaEG sonar.xlsx",
                     range = "Lake Creek sonar!A1:B47") %>%
  dplyr::select(-year) %>%
  unlist(use.names = FALSE)

radios <-
  readxl::read_excel(".\\data-raw\\SusitnaEG sonar.xlsx",
                     range = "radio tags!A1:C47") %>%
  dplyr::mutate(N_lake = ifelse(is.na(LakeCreek_belowsonar + LakeCreek_abovesonar), 0, LakeCreek_belowsonar + LakeCreek_abovesonar),
                x_lake = ifelse(is.na(LakeCreek_abovesonar), 0, LakeCreek_abovesonar)) %>%
  dplyr::select(x_lake, N_lake) %>%
  as.matrix()

sonar <- list(sonar = sonar0, 
              radios = radios)

save(sonar, file=".\\data\\sonar.rda")
