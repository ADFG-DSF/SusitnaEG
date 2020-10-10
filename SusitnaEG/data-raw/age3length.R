d13 <-
readxl::read_xlsx("H:\\My Documents\\DeshkaWeir\\2013-2015\\2013 Results\\2013 Deshka Chinook Age Analysis_DL_AA(2).xlsx",
                  sheet = 3,
                  range = "D3:F283",
                  col_names = c("sex", "length", "age"),
                  col_types = c("text", "numeric", "text"))
lapply(d13, table)
d13$age <- ifelse(grepl("1.1", d13$age), 1.1, d13$age)
d13$year <- 2013

d14 <- 
readxl::read_xlsx("H:\\My Documents\\DeshkaWeir\\2013-2015\\2014 Results\\2014 Deshka Chinook Age Analysis_DL_AA_SH_AA.xlsx",
                  range = "Daryl & Steve Ages!I5:M300",
                  col_names = c("sex", "length", "age"),
                  col_types = c("text", "numeric", rep("skip", 2), "text"))
lapply(d14, table)
d14$age <- ifelse(grepl("1.1", d14$age), 1.1, d14$age)
d14$year <- 2014

d15 <- 
  readxl::read_xlsx("H:\\My Documents\\DeshkaWeir\\2013-2015\\2015 Results\\2015 Deshka Chinook Age Analysis_DL_AA.xlsx",
                    range = "Daryl & Steve Ages!D5:J434",
                    col_names = c("sex", "length", "age"),
                    col_types = c("text", "numeric", rep("skip", 4), "text"))
lapply(d15, table)
d15$age <- ifelse(grepl("1.1", d15$age), 1.1, d15$age)
d15$year <- 2015

d16 <-
readxl::read_xlsx("H:\\My Documents\\DeshkaWeir\\2016\\2016 Deshka Chinook Age Analysis_DL_AR.xlsx",
                  sheet = 4,
                  range = "D5:J567",
                  col_names = c("sex", "length", "age"),
                  col_types = c("text", "numeric", rep("skip", 4), "text"))
lapply(d16, table)
d16$age <- ifelse(grepl("1.1", d16$age), 1.1, d16$age)
d16$year <- 2016

d17 <-
readxl::read_xlsx("H:\\My Documents\\DeshkaWeir\\2017\\2017 Deshka Chinook Age Analysis_DL.xlsx",
                         sheet = 4,
                         range = "D5:I287",
                         col_names = c("sex", "length", "age"),
                         col_types = c("text", "numeric", rep("skip", 3), "text"))
lapply(d17, table)
d17$age <- ifelse(grepl("1.1", d17$age), 1.1, d17$age)
d17$year <- 2017

d18 <-
  readxl::read_xlsx("H:\\My Documents\\DeshkaWeir\\2018\\2018 Deshka Chinook Age Analysis_DL.xlsx",
                    sheet = 4,
                    range = "D5:I248",
                    col_names = c("sex", "length", "age"),
                    col_types = c("text", "numeric", rep("skip", 3), "text"))
lapply(d18, table)
d18$age <- ifelse(grepl("1.1", d18$age), 1.1, d18$age)
d18$year <- 2018

d19 <-
  readxl::read_xlsx("H:\\My Documents\\DeshkaWeir\\2019\\Copy of 2019 Deshka Chinook Age Analysis_DL_AR.xlsx",
                    sheet = 4,
                    range = "F2:J272",
                    col_names = c("length", "sex", "age"),
                    col_types = c("numeric", "text", rep("skip", 2), "text"))
lapply(d19, table)
d19$age <- ifelse(grepl("1.1", d19$age), 1.1, d19$age)
d19$year <- 2019

d20 <-
  readxl::read_xlsx("H:\\My Documents\\DeshkaWeir\\2020\\2020 Deshka Chinook Age Analysis_DL.xlsx", ### have not reviewed
                    sheet = 4,
                    range = "G2:K346",
                    col_names = c("length", "sex", "age"),
                    col_types = c("numeric", "text", rep("skip", 2), "text"))
lapply(d20, table)
d20$age <- ifelse(grepl("1.1", d20$age), 1.1, d20$age)
d20$year <- 2020

rbind(d13, d14, d15, d16, d17, d18, d19, d20) %>%
  dplyr::filter(age %in% c("1.1", "1.2")) %>%
  ggplot2::ggplot(ggplot2::aes(x = length)) + 
    ggplot2::geom_histogram() + 
    ggplot2::geom_vline(ggplot2::aes(xintercept = 500)) +
    ggplot2::facet_grid(age ~ year)

lt500 <- 
  rbind(d13, d14, d15, d16, d17, d18, d19, d20) %>%
  dplyr::filter(age %in% c("1.1", "1.2")) %>%
  dplyr::mutate(small = (length <= 500)) %>%
  dplyr::group_by(year, age) %>%
  dplyr::summarise(n_small = sum(small),
                   n = dplyr::n(),
                   p_small = round(mean(small), 2)) %>%
  dplyr::arrange(age, year)

# lt500 <- lt500_2 %>%
#   dplyr::summarise(n_small = sum(n_small),
#                    n = sum(n),
#                    p_small = n_small / n)

save(lt500, file=".\\SusitnaEG\\data\\lt500.rda")
