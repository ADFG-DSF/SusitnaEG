d13 <-
readxl::read_xlsx("H:\\My Documents\\Deshka R, Chinook and Coho\\2013-2015\\2013 Results\\2013 Deshka Chinook Age Analysis_DL_AA(2).xlsx",
                  sheet = 3,
                  range = "D3:F283",
                  col_names = c("sex", "length", "age"),
                  col_types = c("text", "numeric", "text"))
lapply(d13, table)
d13$age <- ifelse(grepl("1.1", d13$age), 1.1, d13$age)
d13$year <- 2013

d14 <- 
readxl::read_xlsx("H:\\My Documents\\Deshka R, Chinook and Coho\\2013-2015\\2014 Results\\2014 Deshka Chinook Age Analysis_DL_AA_SH_AA.xlsx",
                  range = "Daryl & Steve Ages!I5:M300",
                  col_names = c("sex", "length", "age"),
                  col_types = c("text", "numeric", rep("skip", 2), "text"))
lapply(d14, table)
d14$age <- ifelse(grepl("1.1", d14$age), 1.1, d14$age)
d14$year <- 2014

d15 <- 
  readxl::read_xlsx("H:\\My Documents\\Deshka R, Chinook and Coho\\2013-2015\\2015 Results\\2015 Deshka Chinook Age Analysis_DL_AA.xlsx",
                    range = "Daryl & Steve Ages!D5:J434",
                    col_names = c("sex", "length", "age"),
                    col_types = c("text", "numeric", rep("skip", 4), "text"))
lapply(d15, table)
d15$age <- ifelse(grepl("1.1", d15$age), 1.1, d15$age)
d15$year <- 2015

d16 <-
readxl::read_xlsx("H:\\My Documents\\Deshka R, Chinook and Coho\\2016\\2016 Deshka Chinook Age Analysis_DL_AR.xlsx",
                  sheet = 4,
                  range = "D5:J567",
                  col_names = c("sex", "length", "age"),
                  col_types = c("text", "numeric", rep("skip", 4), "text"))
lapply(d16, table)
d16$age <- ifelse(grepl("1.1", d16$age), 1.1, d16$age)
d16$year <- 2016

d17 <-
readxl::read_xlsx("H:\\My Documents\\Deshka R, Chinook and Coho\\2017\\2017 Deshka Chinook Age Analysis_DL.xlsx",
                         sheet = 4,
                         range = "D5:I287",
                         col_names = c("sex", "length", "age"),
                         col_types = c("text", "numeric", rep("skip", 3), "text"))
lapply(d17, table)
d17$age <- ifelse(grepl("1.1", d17$age), 1.1, d17$age)
d17$year <- 2017

rbind(d13, d14, d15, d16, d17) %>%
  dplyr::filter(age %in% c("1.1", "1.2")) %>%
  ggplot2::ggplot(ggplot2::aes(x = length)) + 
    ggplot2::geom_histogram() + 
    ggplot2::geom_vline(ggplot2::aes(xintercept = 500)) +
    ggplot2::facet_grid(age ~ year)

lt500 <- 
  rbind(d13, d14, d15, d16, d17) %>%
  dplyr::filter(age %in% c("1.1", "1.2")) %>%
  dplyr::mutate(small = (length <= 500)) %>%
  dplyr::group_by(year, age) %>%
  dplyr::summarise(n_small = sum(small),
                   n = n(),
                   p_small = round(mean(small), 2)) %>%
  dplyr::arrange(age, year)

# lt500 <- lt500_2 %>%
#   dplyr::summarise(n_small = sum(n_small),
#                    n = sum(n),
#                    p_small = n_small / n)

devtools::use_data(lt500, pkg = ".\\SusitnaEG", overwrite = TRUE)
