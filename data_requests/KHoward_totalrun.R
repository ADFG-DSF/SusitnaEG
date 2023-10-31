#Most recent posterior (Constrained Bsum after 2020 BOF work session)
post_new <- readRDS(".\\posts\\SuChinook_Bparam_2sf0a9c2.rds")
library(magrittr)
library(SusitnaEG)
id <- get_ids()

#Medians
median <- post$q50$N[40:42,] %>%
  as.data.frame() %>%
  setNames(id$stock_id)

#CV
CV <- post$sd$N[40:42,]/post$mean$N[40:42,] %>%
  as.data.frame() %>%
  setNames(id$stock_id)
  
WriteXLS::WriteXLS(c("median", "CV"), ".\\data_requests\\KHoward_totalrun18-20.xls")
