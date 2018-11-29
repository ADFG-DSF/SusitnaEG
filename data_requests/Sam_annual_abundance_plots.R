library(ggplot2)
library(SusitnaEG)
get_ids()
post <- readRDS(".\\posts\\SuChinook_DsumS_ababd3a.rds")

post$mean$IR %>%
  as.data.frame() %>%
  setNames(stock_id) %>%
  dplyr::select(-Other) %>%
  dplyr::mutate(year = year_id) %>%
  tidyr::gather(stock, IR, -year) %>%
  ggplot(aes(x = year, y = IR, fill = stock)) +
    geom_col() +
    scale_x_discrete(breaks = seq(1979, 2017, 2), name = NULL) +
    labs(y = "Chinnook  salmon", title = "Inriver Run")

post$mean$S %>%
  as.data.frame() %>%
  setNames(stock_id) %>%
  dplyr::select(-Other) %>%
  dplyr::mutate(year = year_id) %>%
  tidyr::gather(stock, IR, -year) %>%
  ggplot(aes(x = year, y = IR, fill = stock)) +
  geom_col() +
  scale_x_discrete(breaks = seq(1979, 2017, 2), name = NULL) +
  labs(y = "Chinnook  salmon", title = "Escapement")
