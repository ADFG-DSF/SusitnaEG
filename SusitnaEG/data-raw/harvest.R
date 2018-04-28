Ha_raw <-
readxl::read_excel(".\\SusitnaEG\\data-raw\\Susitna run reconstruction data_Jan102018.xlsx",
                   range = "Harvest!A1:AO20",
                   col_names = TRUE) %>%
  dplyr::rename(group = Group, trib = X__1) %>%
  dplyr::filter(!is.na(group)) %>%
  tidyr::gather(year, count, -group, -trib) %>%
  dplyr::filter(as.numeric(year) >= 1979)

#SWHS estimated more fisheries in later years.
plot <- Ha_raw %>%
  dplyr::group_by(year, group) %>%
  dplyr::summarise(sum = sum(count, na.rm = TRUE),
                   counts = n(),
                   n = sum(!is.na(count)),
                   pct = n / counts) %>%
  dplyr::mutate(sum = ifelse(n == 0 & sum == 0, NA, sum))

ggplot2::ggplot(plot, ggplot2::aes(x = as.numeric(year), y = pct)) +
  ggplot2::geom_line() +
  ggplot2::facet_grid(group~.)

Ha <- plot %>% 
  dplyr::select(-counts, -n, -pct) %>%
  tidyr::spread(group, sum) %>%
  dplyr::ungroup() %>%
#  dplyr::mutate_all(function(x) ifelse(is.na(x) | x == 0, 1, x)) %>%
    print(n = 100)

devtools::use_data(Ha, pkg = ".\\SusitnaEG", overwrite = TRUE)

Hm <-
  readxl::read_excel(".\\SusitnaEG\\data-raw\\Copy of Susitna Chinook harvest data.xlsx",
                     range = "Sheet1!R3:Y42", 
                     col_names = c("year", "NCI", "NCI_Susitna", "Sub", "Sub_Susitna"),
                     col_types = c(rep("numeric", 3), rep("skip", 3), rep("numeric", 2))) %>%
  dplyr::mutate_all(as.integer) %>%
  dplyr::filter(year >=1979)

Hm$Hm <- rowSums(Hm[, c("NCI", "Sub")], na.rm = TRUE)
Hm$Hm_Susitna <- rowSums(Hm[, c("NCI_Susitna", "Sub_Susitna")], na.rm = TRUE)

devtools::use_data(Hm, pkg = ".\\SusitnaEG", overwrite = TRUE)