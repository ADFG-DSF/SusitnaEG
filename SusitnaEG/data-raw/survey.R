stock <- c(NA, "s5", "s1", "s2", "s3", "s5", "s5", rep("s4", 5))
names(stock) <- codes$code
           

survey_raw <-
readxl::read_excel(".\\SusitnaEG\\data-raw\\Susitna run reconstruction data_Jan102018.xlsx",
                   range = "Aerial counts!A3:AO25") %>%
  dplyr::rename(group = Group, trib = Tributary) %>%
  dplyr::filter(!grepl("weir|Weir", trib),                #drop Deshka Weir counts
                !grepl("non-hatch", trib),                #drop Deception Creek non-hatchery
                !grepl("Other WS", trib),                 #drop porrly defined surveys
                !grepl("Other ES", trib)) %>%            
  tidyr::gather(year, count, -group, -trib) %>%
  dplyr::filter(group != "A") %>%
  dplyr::mutate(stock = stock[group]) %>%
  dplyr::arrange(stock, year) %>%
  dplyr::select(-group)

as <- lapply(unique(survey_raw$stock), 
             function(x) 
               survey_raw %>% dplyr::filter(stock == x) %>% 
               tidyr::spread(trib, count) %>%
               dplyr::mutate_all(function(x) ifelse(x == 0, NA, x)) %>% 
               dplyr::select(-year, -stock) %>%
               as.matrix()
             )
names(as) <- unique(survey_raw$stock)

devtools::use_data(as, pkg = ".\\SusitnaEG", overwrite = TRUE)
