survey_raw <-
readxl::read_excel(".\\SusitnaEG\\data-raw\\Susitna run reconstruction data_Jan102018.xlsx",
                   range = "Aerial counts!A3:AO25") %>%
  dplyr::rename(group = Group, trib = Tributary) %>%
  dplyr::filter(!grepl("weir|Weir", trib),                #drop Deshka Weir counts
                !grepl("non-hatch", trib),                #drop Deception Creek non-hatchery
                !grepl("Other WS", trib),                 #drop porrly defined surveys
                !grepl("Other ES", trib)) %>%            
  tidyr::gather(year, count, -group, -trib) 

#groups E, F, G and K have some years with partial counts within the group
survey_raw %>%
  dplyr::group_by(year, group) %>%
  dplyr::summarise(sum = sum(count, na.rm = TRUE),
                   counts = n(),
                   n = sum(!is.na(count))) %>%
  dplyr::mutate(sum = ifelse(n == 0 & sum == 0, NA, sum)) %>%
  ggplot2::ggplot(ggplot2::aes(x = n)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ..density..), bins = 8) +
    ggplot2::facet_grid(. ~ group) +
    ggplot2::scale_x_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))))

#proportions for each trib by group
reflevels <- sapply(unique(survey_raw$group), function(x) sort(survey_raw$trib[survey_raw$group == x])[1])
mlreg <- function(group_c){
  dat_reg <-
  survey_raw %>%
    dplyr::left_join(survey_raw %>%
                       dplyr::group_by(year, group) %>%
                       dplyr::summarise(total = sum(count)), 
                     c("year", "group")) %>%
    na.omit() %>%
    dplyr::filter(group == group_c) %>%
    dplyr::mutate(trib2 = relevel(as.factor(trib), ref = reflevels[group_c]),
                  year2 = as.numeric(year) - mean(as.numeric(unique(survey_raw$year))),
                  empct = count / total)
  
  test2 <- nnet::multinom(trib2 ~ year2, data = dat_reg, weights = count)
  
  z <- summary(test2)$coefficients/summary(test2)$standard.errors
  wald <- (1 - pnorm(abs(z), 0, 1)) * 2
  preds <- 
    predict(test2, newdata = data.frame(year2 = 1979:2017 - mean(as.numeric(unique(survey_raw$year)))), type = "probs") %>%
    as.data.frame() %>%
    dplyr::mutate(year = 1979:2017) %>%
    tidyr::gather(trib, prob, -year)
  if(length(unique(survey_raw$trib[survey_raw$group == group_c])) == 2){
    preds$trib <- unique(survey_raw$trib[survey_raw$group == group_c])[unique(survey_raw$trib[survey_raw$group == group_c]) != reflevels[group_c]]
    preds2 <- preds
    preds2$trib <- reflevels[group_c]
    preds2$prob <- 1 - preds$prob
    preds <- rbind(preds, preds2)
    preds$year <- as.character(preds$year)
  }

  plot <- 
    ggplot2::ggplot(preds, ggplot2::aes(x = as.numeric(year), y = prob, color = trib)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(data = dat_reg, ggplot2::aes(x = as.numeric(year), y = empct, color = trib))
  
  list(reg = test2, wald = wald, preds = preds, plot = plot)
}
regE <- mlreg("E")
summary(regE[[1]])
regE[[4]]

regF <- mlreg("F")
summary(regF[[1]])
regF[[4]]

regG <- mlreg("G")
summary(regG[[1]])
regG[[4]]

regK <- mlreg("K")
summary(regK[[1]])
regK[[4]]

preds <- rbind(regE[[3]],
               regF[[3]],
               regG[[3]],
               regK[[3]])

missing_groups <- setdiff(codes$code, unique(survey_raw$group))
missing_col <- function(dat, missing){
  for(i in 1:length(missing)) dat[[missing[[i]]]] <- rep(NA, dim(dat)[1])
  dat[order(colnames(dat))]
}

as_expand <-
  dplyr::left_join(survey_raw, preds, by = c("year", "trib")) %>%
  dplyr::filter(!is.na(count) | year == "1980") %>%
  dplyr::group_by(year, group) %>%
  dplyr::summarise(sum_count = sum(count),
                   sum_prob = sum(prob)) %>%
  dplyr::mutate(total = as.integer(ifelse(is.na(sum_prob), sum_count, sum_count / sum_prob))) %>% 
  dplyr::select(year, group, total) %>%
  tidyr::spread(group, total) %>%
  missing_col(missing_groups) %>%
  dplyr::mutate_all(function(x) ifelse(x == 0, NA, x)) %>%
  dplyr::ungroup()

as_complete <-
  survey_raw %>%
  dplyr::group_by(year, group) %>%
  dplyr::summarise(sum_count = sum(count)) %>%
  dplyr::filter(!is.na(sum_count) | year == "1980") %>% 
  dplyr::select(year, group, total = sum_count) %>%
  tidyr::spread(group, total) %>%
  missing_col(missing_groups) %>%
  dplyr::mutate_all(function(x) ifelse(x == 0, NA, x)) %>%
  dplyr::ungroup()

sum(is.na(as_complete))
sum(is.na(as_expand))
prod(dim(as_expand[, -1]))

devtools::use_data(as_complete, pkg = ".\\SusitnaEG", overwrite = TRUE)
devtools::use_data(as_expand, pkg = ".\\SusitnaEG", overwrite = TRUE)

# WriteXLS::WriteXLS(
#   c("codes", "as_complete", "as_expand"),
#   ".\\SusitnaEG\\data-raw\\surveydat_for_steve.xlsx", 
#   SheetNames = c("location codes", "complete as", "expanded as"),
#   na = "NA")
