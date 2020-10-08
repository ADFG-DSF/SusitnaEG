#Assuming age bias between creel and weir may not be warentted
#95% CIs for b in posterior include 0.
#Note
#Willow weir larger age 6 component than willow creel in 2000
#WIllow weir much larger age 6 compnent than Deshka 2000-2002.
age[64:70, ] %>% dplyr::mutate_at(.vars = dplyr::vars(dplyr::starts_with("x")), .funs = dplyr::funs((./n)))

#age data by stock
age %>% 
  dplyr::mutate_at(.vars = dplyr::vars(dplyr::starts_with("x")), .funs = dplyr::funs((./n))) %>%
  dplyr::select(year, stock, n, dplyr::starts_with("x")) %>%
  tidyr::gather(age, prop, -year, -stock, -n) %>%
  ggplot(aes(x = year, y = prop, color = stock)) + 
  geom_point(aes(size = n)) + 
    facet_grid(age ~ .)
  
dat <-
  age %>% #[age$year <= "2001", ] 
  dplyr::mutate(samp = ifelse(grepl("creel|Creel", location), 2, ifelse(grepl("weir|Weir", location), 1, 3))) %>%
  dplyr::select(year, stock, samp, x3, x4, x5, x678) %>%
  tidyr::gather(age, n, -year, -stock, -samp) %>%
  dplyr::group_by(year, stock, samp, age) %>%
  dplyr::summarise(n = sum(n)) %>%
  tidyr::spread(age, n)

#sample effect small when stock included
mod <- nnet::multinom(as.matrix(dat[, grepl("x", names(dat))]) ~ -1 + dat$year + dat$stock + dat$samp)
summary(mod)

mod <- nnet::multinom(as.matrix(dat[, grepl("x", names(dat))]) ~ -1 + dat$year + dat$stock)
summary(mod)
z <- summary(mod)$coefficients/summary(mod)$standard.errors
z
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

#2 level stock factor
dat$stock2 <- factor(dat$stock, labels = c("Deshka" = "West", "East Susitna" = "East", "Talkeetna" = "East", "Yentna" = "West"))
mod <- nnet::multinom(as.matrix(dat[, grepl("x", names(dat))]) ~ -1 + dat$year + dat$stock2)
summary(mod)
z <- summary(mod)$coefficients/summary(mod)$standard.errors
z
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p
