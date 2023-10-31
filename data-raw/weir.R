#Note: update import range annually.
weir <-
  readxl::read_excel(".\\data-raw\\SusitnaEG weirs.xlsx",
                     range = "Annual counts!A1:D46") %>%
  dplyr::select(-year) %>%
  as.matrix()

weir %>%
  as.data.frame() %>%
  mutate(year = 1979:(1978 + dim(weir)[1])) %>%
  pivot_longer(cols = -year, names_to = "population", values_to = "Escapement") %>%
  ggplot(aes(x = year, y = Escapement, color = population)) +
    geom_line()

save(weir, file=".\\data\\weir.rda")
