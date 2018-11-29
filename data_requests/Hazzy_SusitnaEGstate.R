packs <- c("SusitnaEG", "jagsUI")
lapply(packs, require, character.only = TRUE)

rm(list=ls(all=TRUE))

get_ids()

post <- readRDS(".\\posts\\SuChinook_DsumS_ababd3a.rds")

yr0 <- as.numeric(min(year_id)) - 1
yr0_R <- yr0 - age_max

state <- 
  post[["summary"]] %>%
  as.data.frame() %>%
  tibble::rownames_to_column() %>%
  dplyr::filter(grepl("^R\\[|S\\[|N\\[|IR\\[", rowname)) %>%
  dplyr::mutate(name = factor(gsub("^(.*)\\[\\d+,\\d\\]", "\\1", rowname),
                              levels = c("N", "IR", "S", "R"),
                              labels = c("Total Run", "Inriver Run", "Escapement", "Recruitment")),
                index = as.numeric(gsub("^.*\\[(\\d+),\\d\\]", "\\1", rowname)),
                year = (name != c("Recruitment")) * (yr0 + index) + (name == "Recruitment") * (yr0_R + index),
                stock = factor(stock_id[gsub("^.*\\[\\d+,(\\d)\\]", "\\1", rowname)], levels = stock_id),
                cv = sd/mean) %>%
  dplyr::select(name, stock, year, mean, sd, dplyr::ends_with("%"))

WriteXLS::WriteXLS(state, ExcelFileName = "H:\\My Documents\\SusitnaEG\\data_requests\\Hazzy_SusitnaEGstate.xlsx")
