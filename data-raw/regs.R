#Note: update import range annually.
library(magrittr)

regs <-
  readxl::read_excel(".\\data-raw\\SusitnaEG regs.xlsx",
                     range = "reg bins!A1:E47",
                     col_names = TRUE,
                     col_types = c("numeric", rep("text", 4))) %>%
  tidyr::gather(stock, reg, -year) %>%
  dplyr::mutate(stock = factor(stock, levels = c("Deshka", "East Susitna", "Talkeetna", "Yentna", "Other")),
                reg = factor(reg, levels = 1:4, labels = c("Closed", "Restricted", "No Action", "Liberalized")))

save(regs, file=".\\data\\regs.rda")
