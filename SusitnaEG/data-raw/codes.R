name = c("Alexander Cr.",
         "Susitna rm34_102",
         "Deshka R.",
         "",
         "East Susitna tribs",
         "Talkeetna R.",
         "Susitna rm102_153",
         "Chulitna R.",
         "",
         "Lake Cr.",
         "Kahitltna R.",
         "Talachulitna R.",
         "Skwentna R.",
         "Other Yentna R.")
names(name) <- LETTERS[1:which(LETTERS == "N")]

codes <-
readxl::read_excel(".\\SusitnaEG\\data-raw\\Susitna run reconstruction data_Jan102018.xlsx",
                   range = "Group codes!A2:D15",
                   col_names = c("code", "mrcode", "name_long"),
                   col_types = c("skip", rep("text", 3)),
                   na = c("", "N/A")) %>%
  dplyr::filter(!is.na(code)) %>%
  dplyr::mutate(name = name[code],
                drainage = ifelse(code %in% LETTERS[1:which(LETTERS == "H")], "Susitna R.", "Yentna R.")) %>%
  dplyr::select(-name_long)

devtools::use_data(codes, pkg = ".\\SusitnaEG", overwrite = TRUE)
