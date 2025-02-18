# brood and state tables for the NOAA GOA_ESA data request for brood tables

# Author: Adam Reimer
# Version: 2024-09-09

# Packages
packs <- c("tidyverse", "writexl")
lapply(packs, require, character.only = TRUE)

source(".\\functions\\table.R")
source(".\\functions\\get.R")
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Read data ---------------------------------------------------------------
post <- readRDS(".\\posts\\SuChinook_10242023.rds")
get_ids(year_range = 1979:2023)

#  * Format data ----------------------------------------------------------

# Analysis ----------------------------------------------------------------
brood_deshka <- table_brood(post, 1)
brood_east <- table_brood(post, 2)
brood_talkeetna <- table_brood(post, 3)
brood_yentna <- table_brood(post, 4)

table_state2 <- function(post_dat, display){
  stopifnot(exists("year_id", .GlobalEnv),
            exists("stock_id", .GlobalEnv),
            exists("age_max", .GlobalEnv))
  yr0 <- as.numeric(min(year_id)) - 1
  yr0_R <- yr0 - age_max
  
  temp <- 
    post_dat[["summary"]] %>%
    as.data.frame() %>%
    dplyr::select(mean, sd, median = "50%") %>%
    tibble::rownames_to_column() %>%
    dplyr::filter(grepl("^R\\[|S\\[|N\\[", rowname)) %>%
    dplyr::mutate(name = gsub("^(.*)\\[\\d+,\\d\\]", "\\1", rowname),
                  index = as.numeric(gsub("^.*\\[(\\d+),\\d\\]", "\\1", rowname)),
                  year = ifelse(gsub("^(.*)\\[\\d+,\\d\\]", "\\1", rowname) == "R", (yr0_R + index), (yr0 + index)),
                  stock = factor(stock_id[gsub("^.*\\[\\d+,(\\d)\\]", "\\1", rowname)], 
                                 levels = stock_id,
                                 labels = paste0(stock_id)),
                  cv = sd/mean)
  
  temp2 <-
    temp %>%
    dplyr::select(year, name, stock, median, cv) %>%
    tidyr::pivot_wider(names_from = name, values_from = c(median, cv)) %>%
    dplyr::arrange(stock, year) %>%
    dplyr::rename(Year = year)
  
  out <- split(temp2, temp2$stock)
}

state <- table_state2(post, "bystock")


# Results -----------------------------------------------------------------
write_xlsx(list("Deshka" = brood_deshka,
                "East_Susitna" = brood_east,
                "Talkeetna" = brood_talkeetna,
                "Yentna" = brood_yentna),
           ".\\data_requests\\GOA_ESA_brood.xlsx")

write_xlsx(list("Deshka" = state[[1]],
                "East_Susitna" = state[[2]],
                "Talkeetna" = state[[3]],
                "Yentna" = state[[4]]),
           ".\\data_requests\\GOA_ESA_state.xlsx")
