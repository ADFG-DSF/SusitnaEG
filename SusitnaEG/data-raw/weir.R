deshka <-
readxl::read_excel(".\\SusitnaEG\\data-raw\\Susitna run reconstruction data_Jan102018.xlsx",
                   range = "Aerial counts!B3:AO25") %>%
  dplyr::rename(trib = Tributary) %>%
  dplyr::filter(grepl("weir|Weir", trib)) %>%            
  tidyr::gather(year, count, -trib) %>%
  dplyr::mutate(trib = gsub(" weir| Weir","", trib),
                count = ifelse(year == "1998", NA, count)) #In 1998, weir < survey. per Nick, 1998 weir count incomplete (missed early season)

montana <-
  data.frame(trib = rep("Montana", 2),
             year = c(2013, 2014),
             count = c(2015, 1217))
willow <-
  readxl::read_excel(".\\SusitnaEG\\data-raw\\Copy of Willow Creek weir data.xlsx",
                     range = "Sheet1!A18:D76") %>%
  tidyr::gather(year, count, -Date) %>% 
  dplyr::group_by(year) %>%
  dplyr::summarise(count = sum(count, na.rm = TRUE)) %>%
  dplyr::mutate(trib = "Willow")
             
weir <- 
  rbind(deshka, montana, willow) %>%
  tidyr::spread(trib, count) %>%
  dplyr::select(-year) %>%
  tibble::remove_rownames() %>%
  as.matrix()

devtools::use_data(weir, pkg = ".\\SusitnaEG", overwrite = TRUE)

