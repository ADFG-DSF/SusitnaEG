# Extract total run and age comp estimates for Susitna River Chinook salmon for the Owl Ridge Beluga prey study

# Author: Adam Reimer
# Version: 2023-04-27

# Packages
packs <- c("tidyverse", "xlsx")
lapply(packs, require, character.only = TRUE)
  
# Key Parameters
# N.t.reported$N.t = Total run reported in FM 20-01
# N.t.unreported$N.t = Total run produced after 2022 season and used for 2022 EG analysis
# q.reported$q = Age composition reported in FM 20-01
# q.unreported$q = Age composition produced after 2022 season and used for 2022 EG analysis
# asl = Deshka River Chinook salmon age, sex and length data, 2013-2022

# ============================================================================

# Code --------------------------------------------------------------------
years <- 1979:2022
get_est <- function(post, param, year_id = years){
  post$q50[param] %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    mutate(year = years[as.numeric(gsub("^.*\\[(\\d+),\\d\\]", "\\1", rowname))]) %>%
    select(-rowname)
}


# posteriors --------------------------------------------------------------

post_1979to2017 <- readRDS(".\\posts\\SuChinook_134cf92.rds")
post_1979to2022 <- readRDS(".\\posts\\SuChinook_11302022.rds")


# Total Run ---------------------------------------------------------------

N.t.reported <- 
  get_est(post_1979to2017, "N.ta") %>% 
  mutate(N.t = rowSums(across(starts_with("N.ta")))) %>%
  select(year, N.t)

N.t.unreported <- 
  get_est(post_1979to2022, "N.ta") %>% 
  mutate(N.t = rowSums(across(starts_with("N.ta")))) %>%
  select(year, N.t)

plot(N.t.unreported$year, N.t.unreported$N.t, type = "line")
lines(N.t.reported$year, N.t.reported$N.t)


# Age Composition ---------------------------------------------------------

q.reported <- 
  get_est(post_1979to2017, "q") %>%
  setNames(c("age_3", "age_4", "age_5", "age_6", "year"))

q.unreported <- 
  get_est(post_1979to2022, "q") %>%
  setNames(c("age_3", "age_4", "age_5", "age_6", "year"))


# ASL data ----------------------------------------------------------------

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
  readxl::read_xlsx("H:\\My Documents\\DeshkaWeir\\2020\\2020 Deshka Chinook Age Analysis_DL.xlsx",
                    sheet = 4,
                    range = "G2:K346",
                    col_names = c("length", "sex", "age"),
                    col_types = c("numeric", "text", rep("skip", 2), "text"))
lapply(d20, table)
d20$age <- ifelse(grepl("1.1", d20$age), 1.1, d20$age)
d20$year <- 2020

d21 <-
  readxl::read_xlsx("H:\\My Documents\\DeshkaWeir\\2021\\Deshka Chinook Age Analysis 2021_DL.xlsx",
                    sheet = 4,
                    range = "G2:N371",
                    col_names = c("length", "sex", "age"),
                    col_types = c("numeric", "text", rep("skip", 5), "text"))
lapply(d21, table)
d21$age <- ifelse(grepl("1.1", d21$age), 1.1, d21$age)
d21$year <- 2021

d22 <-
  readxl::read_xlsx("H:\\My Documents\\DeshkaWeir\\2022\\Copy of Deshka Chinook Age Analysis_LFW _Modified.xlsx",
                    sheet = 4,
                    range = "G2:M130",
                    col_names = c("length", "sex", "age"),
                    col_types = c("numeric", "text", rep("skip", 4), "text"))
lapply(d22, table)
d22$age <- ifelse(grepl("1.1", d22$age), 1.1, d22$age)
d22$year <- 2022

asl <- rbind(d13, d14, d15, d16, d17, d18, d19, d20, d21, d22)


# Output ------------------------------------------------------------------

write.xlsx(N.t.reported, ".\\data_requests\\Beluga_Susitna_Chinook.xlsx", "N.t.1979-2017", row.names = FALSE)
write.xlsx(N.t.unreported[N.t.unreported$year %in% 2018:2022, ], 
           ".\\data_requests\\Beluga_Susitna_Chinook.xlsx", 
           "N.t.2018-2022", 
           append=TRUE, 
           row.names = FALSE)
write.xlsx(q.reported, ".\\data_requests\\Beluga_Susitna_Chinook.xlsx", "q.1979-2017", append=TRUE, row.names = FALSE)
write.xlsx(q.unreported[q.unreported$year %in% 2018:2022, ], 
           ".\\data_requests\\Beluga_Susitna_Chinook.xlsx", 
           "q.2018-2022", 
           append=TRUE, 
           row.names = FALSE)
write.xlsx(asl, ".\\data_requests\\Beluga_Susitna_Chinook.xlsx", "asl.2013-2022", append=TRUE)

