mr <- data.frame(mr_yentna =   c(rep(NA, 35), 22267, 48400, 31310, 17804), 
                 mr_mainstem = c(rep(NA, 34), 89463, 68225, 88600, 66116, 45471))

mr$cv_yentna <- rep(0.25, dim(mr)[1])
mr$cv_mainstem <- rep(0.15, dim(mr)[1])

devtools::use_data(mr, pkg = ".\\SusitnaEG", overwrite = TRUE)
