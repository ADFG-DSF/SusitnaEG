age_deshka <-
  readxl::read_excel(".\\SusitnaEG\\data-raw\\SusitnaEG age.xlsx",
                     range = "Deshka!A11:I52",
                     col_names = c("year", "p3", "p4", "p5", "p6", "p78", "pall", "n", "source"),
                     col_types = c("text", rep("numeric", 7), "text")) %>%
  dplyr::filter(!is.na(n)) %>%  # 1990 no sampling
  dplyr::mutate(x3 = as.integer(p3 * n),
                x4 = as.integer(p4 * n),
                x5 = as.integer(p5 * n),
                x6 = as.integer(p6 * n),
                x78 = as.integer(p78 *n),
                location = ifelse(grepl("creel|Creel", source), "Deshka harvest", "Deshka weir"),
                stock = "Deshka") %>%
  dplyr::select(-dplyr::starts_with("p"), -source) 
age_deshkaearly <- age_deshka %>% dplyr::filter(year < "1986") %>% dplyr::select(-location, -stock)  # <1986 data dulicated in age_alex

age_yentna <-
  readxl::read_excel(".\\SusitnaEG\\data-raw\\SusitnaEG age.xlsx",
                     range = "Alexander_Deshka_Yentna sport!A6:K12",
                     col_types = c("text", "skip", "text", rep("numeric", 6), "skip", "numeric"),
                     col_names = c("year", "location", "p3", "p4", "p5", "p6", "p7", "p6_2", "n")) %>%
  dplyr::mutate_all(function(x) ifelse(is.na(x), 0, x)) %>%
  dplyr::mutate(p6 = p6 + p6_2,
                x3 = as.integer(p3 / 100 * n),
                x4 = as.integer(p4 / 100  * n),
                x5 = as.integer(p5 / 100  * n),
                x6 = as.integer(p6 / 100  * n),
                x78 = as.integer(p7 / 100  * n),
                location = ifelse(grepl("combined", location), gsub("combined", "harvest", location), paste0(location, " harvest")),
                stock = "Yentna") %>%
  dplyr::select(-dplyr::starts_with("p")) %>%
  dplyr::filter(n != 0) %>%
  dplyr::left_join(age_deshkaearly, by = "year") %>%
  dplyr::mutate(x3 = x3.x - x3.y,
                x4 = x4.x - x4.y,
                x5 = x5.x - x5.y,
                x6 = x6.x - x6.y,
                x78 = x78.x - x78.y,
                n = n.x - n.y) %>%
  dplyr::select(-dplyr::ends_with(".x"), -dplyr::ends_with(".y")) %>%
  dplyr::filter(!(n == 0)) %>%
  dplyr::mutate(location = gsub("Deshka, (.*)", "\\1", location))

age_east <-
  readxl::read_excel(".\\SusitnaEG\\data-raw\\SusitnaEG age.xlsx",
                     range = "Eastside_Talkeetna sport!A6:K44",
                     col_types = c("text", "skip", "text", rep("numeric", 5), rep("skip", 2), "numeric"),
                     col_names = c("year", "location", "p3", "p4", "p5", "p6", "p7", "n")) %>%
  dplyr::mutate_all(function(x) ifelse(is.na(x), 0, x)) %>%
  dplyr::mutate(n = ifelse(n == 0, 100, n),
                x3 = as.integer(p3 / 100 * n),
                x4 = as.integer(p4 / 100  * n),
                x5 = as.integer(p5 / 100  * n),
                x6 = as.integer(p6 / 100  * n),
                x78 = as.integer(p7 / 100  * n),
                location = paste0(location, " harvest"),
                stock = ifelse(grepl("Talkeetna", location), "Talkeetna", "East Susitna")) %>%
  dplyr::select(-dplyr::starts_with("p"))

age_willow <-
  readxl::read_excel(".\\SusitnaEG\\data-raw\\SusitnaEG age.xlsx",
                     range = "Willow weir!A6:K8",
                     col_types = c("text", "skip", "text", rep("numeric", 5), rep("skip", 2), "numeric"),
                     col_names = c("year", "location", "p3", "p4", "p5", "p6", "p7", "n")) %>%
  dplyr::mutate_all(function(x) ifelse(is.na(x), 0, x)) %>%
  dplyr::mutate(x3 = as.integer(p3 / 100 * n),
                x4 = as.integer(p4 / 100  * n),
                x5 = as.integer(p5 / 100  * n),
                x6 = as.integer(p6 / 100  * n),
                x78 = as.integer(p7 / 100  * n),
                location = paste0(location, " weir"),
                stock = "East Susitna") %>%
  dplyr::select(-dplyr::starts_with("p"))

xage <- 
  dplyr::bind_rows(age_deshka, age_yentna, age_east, age_willow) %>%
  dplyr::mutate(x678 = x6 + x78,
                n = x3 + x4 + x5 + x678,
                p3 = round(x3 / n, 2),
                p4 = round(x4 / n, 2),
                p5 = round(x5 / n, 2),
                p678 = round(x678 / n, 2)) %>%
  dplyr::select(year, location, stock, x3, x4, x5, x678, n, p3, p4, p5, p678) %>%
  dplyr::arrange(year, location) 

library(ggplot2)
xage %>% 
  dplyr::select(year, stock, n, dplyr::starts_with("p")) %>%
  tidyr::gather(age, prop, -year, -stock, -n) %>%
  ggplot(aes(x = year, y = prop, color = stock)) + 
    geom_point(aes(size = n)) + facet_grid(age ~ .)

age <- 
  xage %>% 
  dplyr::select(-dplyr::starts_with("p"))
save(age, file=".\\SusitnaEG\\data\\age.rda")
