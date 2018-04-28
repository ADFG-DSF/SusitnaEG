deshka <-
readxl::read_excel(".\\SusitnaEG\\data-raw\\Susitna run reconstruction data_Jan102018.xlsx",
                   range = "Aerial counts!A3:AO25") %>%
  dplyr::rename(group = Group, trib = Tributary) %>%
  dplyr::filter(grepl("weir|Weir", trib)) %>%            
  tidyr::gather(year, count, -group, -trib) 

montana <-
  data.frame(group = c("E", "E"),
             trib = rep("Montana weir", 2),
             year = c(2013, 2014),
             count = c(2015, 1217))
             
weir <- rbind(deshka, montana)

devtools::use_data(weir, pkg = ".\\SusitnaEG", overwrite = TRUE)

