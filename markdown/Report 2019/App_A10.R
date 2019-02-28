library(SusitnaEG)
app_A10 <- 
  mapply(function(x, y){paste0(format(round(x, 0), scientific = FALSE, digits = 0, big.mark = ","), " (", format(round(y, 2), digits = 2), ")")}, 
       as.data.frame(cbind(Ha[[1]], Deshka_up = Hd$H)), as.data.frame(cbind(Ha[[2]], Deshka_up = Hd$cv)), SIMPLIFY = FALSE) %>% 
  do.call(cbind, .) %>%
  as.data.frame() %>%
  dplyr::select(Deshka, Deshka_up, 'East Susitna', Talkeetna, Yentna)

WriteXLS::WriteXLS(app_A10, ".\\markdown\\Report 2019\\app_A10.xls")
