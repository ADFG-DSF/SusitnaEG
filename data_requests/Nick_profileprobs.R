library(SusitnaEG)
post <- readRDS(".\\posts\\SuChinook_a02eb71a.rds")
get_ids()
profiles <- lapply(stock_id, get_profile, post_dat = post) %>% setNames(stock_id)

get_profileprobs2 <- function(profile_dat){
  profile_dat %>% 
    dplyr::mutate(s2 = cut(s, breaks = seq(-50, round(max(s), -2) + 50, by = 100), labels = seq(-50, round(max(s), -2), by = 100) + 50)) %>%
    dplyr::select(-name, -s, -S.msy, -SY) %>%
    dplyr::group_by(s2) %>%
    dplyr::summarise_all(mean, na.rm = TRUE) %>%
    dplyr::filter(!(OYP80 <= 0.5 & as.numeric(s2) < as.numeric(s2[which.max(ORP80)])) & 
                  !(ORP90 <= 0.5 & as.numeric(s2) > as.numeric(s2[which.max(ORP90)])))   
}
out <- lapply(profiles, get_profileprobs2) 
WriteXLS::WriteXLS(out, ".\\data_requests\\Nick_profileprobs_dec1018.xlsx")
