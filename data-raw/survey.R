#Note: update import range annually.
library(tidyverse)

lut <- data.frame(stock = c("Deshka", rep("East_Susitna", 7), rep("Talkeetna", 2), rep("Yentna", 4), rep("Other", 3)),
                  trib = c("Deshka", "Deception", "Goose", "Kashwitna", "Little Willow", "Montana", "Sheep", "Willow", "Clear", "Prairie", 
                           "Cache", "Lake", "Peters", "Talachulitna", "Chulitna", "Indian", "Portage"),
                  stringsAsFactors = FALSE)

survey_raw <-
  readxl::read_excel(".\\data-raw\\SusitnaEG survey.xlsx",
                     range = "Single aerial survey counts!A5:X50",
                     col_names = c("year", "Alexander", "skip", 
                                   "Deshka", "skip",
                                   "Deception", "Goose", "Kashwitna", "Little Willow", "Montana", "Sheep", "Willow", "skip",
                                   "Clear", "Prairie", "skip", 
                                   "Cache", "Lake", "Peters", "Talachulitna", "skip",
                                   "Chulitna", "Indian", "Portage"), 
                     col_types = c(rep("numeric", 2), "skip",
                                   "numeric", "skip",
                                   rep("numeric", 7), "skip",
                                   rep("numeric", 2), "skip",
                                   rep("numeric", 4), "skip",
                                   rep("numeric", 3))) %>%
  dplyr::mutate(Willow = Willow + Deception) %>%
  dplyr::select(-Alexander, -Deception, -dplyr::starts_with("skip")) %>%
  tidyr::gather(trib, count, -year) %>%
  dplyr::left_join(lut, by = "trib")

make_list <- function(stock){
  survey_raw[survey_raw$stock == stock, ] %>%
    dplyr::select(-stock) %>%
    tidyr::spread(trib, count) %>%
    dplyr::select(-year) %>%
    as.matrix()
}

as <- list(Deshka = make_list("Deshka"),
           'East Susitna' = make_list("East_Susitna"),
           Talkeetna = make_list("Talkeetna"),
           Yentna = make_list("Yentna"))

#time series of survey counts
as[[2]] %>%
  as.data.frame() %>%
  mutate(year = 1979:(1978 + dim(as[[2]])[1])) %>%
  pivot_longer(cols = -year, names_to = "population", values_to = "Count") %>%
  ggplot(aes(x = year, y = Count, color = population)) +
  geom_line()

as[[3]] %>%
  as.data.frame() %>%
  mutate(year = 1979:(1978 + dim(as[[3]])[1])) %>%
  pivot_longer(cols = -year, names_to = "population", values_to = "Count") %>%
  ggplot(aes(x = year, y = Count, color = population)) +
  geom_line()

as[[4]] %>%
  as.data.frame() %>%
  mutate(year = 1979:(1978 + dim(as[[4]])[1])) %>%
  pivot_longer(cols = -year, names_to = "population", values_to = "Count") %>%
  ggplot(aes(x = year, y = Count, color = population)) +
  geom_line()

# 2024 count as a percentage of the historical median
as[[2]][dim(as[[2]])[1], ]/apply(as[[2]], MARGIN = 2, FUN = median, na.rm = TRUE)
as[[3]][dim(as[[3]])[1], ]/apply(as[[3]], MARGIN = 2, FUN = median, na.rm = TRUE)
as[[4]][dim(as[[4]])[1], ]/apply(as[[4]], MARGIN = 2, FUN = median, na.rm = TRUE)

#2024 count as a percentage of the previous minimum
as[[2]][dim(as[[2]])[1], ]/apply(as[[2]][1:(dim(as[[2]])[1] - 1), ], MARGIN = 2, FUN = min, na.rm = TRUE)
as[[3]][dim(as[[4]])[1], ]/apply(as[[3]][1:(dim(as[[3]])[1] - 1), ], MARGIN = 2, FUN = min, na.rm = TRUE)
as[[4]][dim(as[[4]])[1], ]/apply(as[[4]][1:(dim(as[[4]])[1] - 1), ], MARGIN = 2, FUN = min, na.rm = TRUE)

save(as, file=".\\data\\as.rda")

