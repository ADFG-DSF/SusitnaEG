library(SusitnaEG)
get_ids()

survey_raw <-
readxl::read_excel(".\\SusitnaEG\\data-raw\\Susitna run reconstruction data_Jan102018.xlsx",
                   range = "Aerial counts!A3:AO25") %>%
  dplyr::rename(trib0 = Tributary) %>%
  dplyr::filter(!grepl("weir|Weir", trib0),                #drop Deshka Weir counts
                !grepl("non-hatch", trib0),                #drop Deception Creek non-hatchery
                !grepl("Other WS", trib0),                 #drop poorly defined surveys
                !grepl("Other ES", trib0)) %>%            
  tidyr::gather(year, count, -stock, -trib0) %>%
  dplyr::filter(!is.na(stock)) %>%
  dplyr::mutate(stock = factor(stock, stock_id),
                trib0 = gsub(" Aerial| River| Creek.*", "", trib0),
                trib = ifelse(trib0 == "Deception", "Willow", trib0)) %>%
  dplyr::group_by(year, stock, trib) %>%
  dplyr::summarise(count = sum(count)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(stock, year)

as <- lapply(unique(survey_raw$stock), 
             function(x) 
               survey_raw %>% dplyr::filter(stock == x) %>% 
               tidyr::spread(trib, count) %>%
               dplyr::mutate_all(function(x) ifelse(x == 0, NA, x)) %>% 
               dplyr::select(-year, -stock) %>%
               as.matrix()
             )
names(as) <- unique(survey_raw$stock)

stopifnot(names(as) %in% stock_id,
          colnames(as[["Deshka"]]) %in% trib_id[["Deshka"]],
          colnames(as[["East Susitna"]]) %in% trib_id[["East Susitna"]],
          colnames(as[["Talkeetna"]]) %in% trib_id[["Talkeetna"]],
          colnames(as[["Yentna"]]) %in% trib_id[["Yentna"]],
          colnames(as[["Other"]]) %in% trib_id[["Other"]])

devtools::use_data(as, pkg = ".\\SusitnaEG", overwrite = TRUE)
