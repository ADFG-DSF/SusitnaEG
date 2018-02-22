harvest_raw <-
readxl::read_excel(".\\SusitnaEG\\data-raw\\Susitna run reconstruction data_Jan102018.xlsx",
                   range = "Harvest!A1:AO20",
                   col_names = TRUE) %>%
  dplyr::rename(group = Group, trib = X__1) %>%
  dplyr::filter(!is.na(group)) %>%
  tidyr::gather(year, count, -group, -trib) %>%
  dplyr::filter(as.numeric(year) >= 1979)

#groups B, E, K and N have some years with partial counts within the group
harvest_raw %>%
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
reflevels <- sapply(unique(harvest_raw$group), function(x) sort(harvest_raw$trib[harvest_raw$group == x])[1])
mlreg <- function(dat, group_c){
  dat_reg <-
  dat %>%
    dplyr::left_join(dat %>%
                       dplyr::group_by(year, group) %>%
                       dplyr::summarise(total = sum(count)), 
                     c("year", "group")) %>%
    na.omit() %>%
    dplyr::filter(group == group_c) %>%
    dplyr::mutate(trib2 = relevel(as.factor(trib), ref = reflevels[group_c]),
                  year2 = as.numeric(year) - mean(as.numeric(unique(dat$year))))
  
  test2 <- nnet::multinom(trib2 ~ year2, data = dat_reg, weights = count)
  
  z <- summary(test2)$coefficients/summary(test2)$standard.errors
  wald <- (1 - pnorm(abs(z), 0, 1)) * 2
  preds <- 
    predict(test2, newdata = data.frame(year2 = 1979:2017 - mean(as.numeric(unique(dat$year)))), type = "probs") %>%
    as.data.frame() %>%
    dplyr::mutate(year = 1979:2017) %>%
    tidyr::gather(trib, prob, -year)
  if(length(unique(dat$trib[dat$group == group_c])) == 2){
    preds$trib <- unique(dat$trib[dat$group == group_c])[unique(dat$trib[dat$group == group_c]) != reflevels[group_c]]
    preds2 <- preds
    preds2$trib <- reflevels[group_c]
    preds2$prob <- 1 - preds$prob
    preds <- rbind(preds, preds2)
    preds$year <- as.character(preds$year)
  }

  plot <- 
    ggplot2::ggplot(preds, ggplot2::aes(x = as.numeric(year), y = prob, color = trib)) +
    ggplot2::geom_line()
  
  list(reg = test2, wald = wald, preds = preds, plot = plot)
}
regB <- mlreg(harvest_raw, "B")
summary(regB[[1]])
regB[[4]]

regE <- mlreg(harvest_raw, "E")
summary(regE[[1]])
regE[[4]]

regN <- mlreg(harvest_raw, "N")
summary(regN[[1]])
regN[[4]]

preds <- rbind(regB[[3]],
               regE[[3]],
               regN[[3]])

missing_groups <- setdiff(codes$code, unique(harvest_raw$group))
missing_col <- function(dat, missing){
  for(i in 1:length(missing)) dat[[missing[[i]]]] <- rep(NA, dim(dat)[1])
  dat[order(colnames(dat))]
}

Ha_expand <-
  dplyr::left_join(harvest_raw, preds, by = c("year", "trib")) %>%
  dplyr::filter(!is.na(count)) %>%
  dplyr::group_by(year, group) %>%
  dplyr::summarise(sum_count = sum(count),
                   sum_prob = sum(prob)) %>%
  dplyr::mutate(total = as.integer(ifelse(is.na(sum_prob), sum_count, sum_count / sum_prob))) %>% 
  dplyr::select(year, group, total) %>%
  tidyr::spread(group, total) %>%
  missing_col(missing_groups)

Ha_complete <-
  harvest_raw %>%
  dplyr::group_by(year, group) %>%
  dplyr::summarise(sum_count = sum(count)) %>%
  dplyr::filter(!is.na(sum_count)) %>% 
  dplyr::select(year, group, total = sum_count) %>%
  tidyr::spread(group, total) %>%
  missing_col(missing_groups)

sum(is.na(Ha_complete))
sum(is.na(Ha_expand))
prod(dim(Ha_expand[, -1]))

devtools::use_data(Ha_complete, pkg = ".\\SusitnaEG", overwrite = TRUE)
devtools::use_data(Ha_expand, pkg = ".\\SusitnaEG", overwrite = TRUE)

WriteXLS::WriteXLS(
  c("codes", "Ha_complete", "Ha_expand"),
  ".\\SusitnaEG\\data-raw\\harvestdat_for_steve.xlsx", 
  SheetNames = c("location codes", "complete Ha", "expanded Ha"),
  na = "NA")
