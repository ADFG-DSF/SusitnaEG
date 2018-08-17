#' Plots of composition and abundance by age
#'
#' Faceted plot of age at maturity, age composition and total run by age.  Observed age composition is also plotted.
#'
#' @param input_dat The input dataset for the SRA model
#' @param stats_dat The output from get_summary() for the SRA model mcmc.list ouput
#'
#' @return A figure
#'
#' @examples
#' x.a <- 
#' age[grepl("Deshka", age$location), ] %>%
#'  dplyr::mutate(x34 = x3 + x4,
#'                x678 = x6 + x78) %>%
#'  dplyr::select(x34, x5, x678) %>%
#'  as.matrix()
#'
#' plot_age(x.a, get_summary(post))
#'
#' @export
plot_age <- function(input_dat, stats_dat){
  stopifnot(exists("year_id", .GlobalEnv),
            exists("age_max", .GlobalEnv))
  yr0 <- as.numeric(min(year_id)) - 1
  yr0_p <- yr0 - age_max
  
Q.obs <- as.data.frame(input_dat / rowSums(input_dat)) %>%
  setNames(paste0("age", 1:ncol(input_dat))) %>%
  tibble::rownames_to_column(var = "yr_id") %>%
  tidyr::gather(age, prop, dplyr::starts_with("age")) %>%
  dplyr::group_by(yr_id) %>%
  dplyr::mutate(prop = cumsum(prop), plot = "Age Composition") %>%
  dplyr::ungroup(yr_id) %>%
  dplyr::mutate(year = as.numeric(year_id[a$yr.a[as.numeric(yr_id)]]))
                
P.mn <- get_array(stats_dat, "p") %>%
  tidyr::gather(age, prop, dplyr::starts_with("age")) %>%
  dplyr::mutate(plot = "Age-at-Maturity") %>%
  dplyr::rename(year = byear)
Q.mn <- get_array(stats_dat, "q") %>%
  tidyr::gather(age, prop, dplyr::starts_with("age")) %>%
  dplyr::mutate(plot = "Age Composition") %>%
  dplyr::rename(year = cyear)
N.mn <- get_array(stats_dat, "N.ta") %>%
  tidyr::gather(age, prop, dplyr::starts_with("age")) %>%
  dplyr::mutate(plot = "Total Run") %>%
  dplyr::rename(year = cyear)

dplyr::bind_rows(P.mn, Q.mn, N.mn) %>%
  dplyr::mutate(year = (plot != c("Age-at-Maturity")) * (yr0 + year) + (plot == c("Age-at-Maturity")) * (yr0_p + year)) %>%
  ggplot2::ggplot(ggplot2::aes(x = year, y = prop, alpha = age)) +
    ggplot2::geom_area(position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::facet_grid(plot ~ ., scales = "free", switch = "y") +
    ggplot2::scale_x_continuous(breaks = seq(yr0_p, max(year_id), 3), minor_breaks = NULL) +
    ggplot2::scale_y_continuous(minor_breaks = NULL, labels = scales::comma) +
    ggplot2::geom_point(data = Q.obs, size = 3) +
    ggplot2::scale_alpha_discrete(name = NULL, labels = names(age_id)) +
    ggplot2::labs(y = NULL, x = "Year") +
    ggplot2::theme_bw() +
    ggplot2::theme(strip.background = ggplot2::element_rect(colour="white", fill="white"), strip.placement = "outside")
}


#' OYP, ORP and/or OFP plots based on aerial counts
#'
#' Produces a faceted plot of OYP, ORP or OFP with an overlay of the proposed goal range.
#'
#' @param profile_dat Output of the get_profile function
#' @param limit Upper bound of spawners for plot. Default (NULL) will use the maximum observed count.
#' @param goal_range A vector with two element c(lower_bound, upper_bound). Defaults to NA.
#' @param profiles the profiles to plot as a character vector e.g. c("OYP", "OFP").  Defaults to c("OYP", "ORP", "OFP").
#'
#' @return A figure
#'
#' @examples
#' plot_countprofile(get_countprofile(post, 200000), c("OYP", "OFP"))
#'
#' @export
plot_countprofile <- function(profile_dat, limit = NULL, goal_range = NA, profiles = c("OYP", "ORP", "OFP")){
  stopifnot(exists("stock_id", .GlobalEnv),
            "package:SusitnaEG" %in% search())
  
  temp <-unlist(lapply(profiles, function(x){paste0(x, c("70", "80", "90"))}))
  profile_label <- ggplot2::as_labeller(c('OYP' = "Optimum Yield Profile",
                                          'OFP' = "Overfishing Profile",
                                          'ORP' = "Optimum Recruitment Profile"))
  
  if(is.null(limit)){
    xmax <- max(as[[unique(profile_dat$stock_name)]][, unique(profile_dat$trib_name)], na.rm = TRUE)
  }
  else xmax <- limit
  
  plot <- 
    profile_dat %>%
    dplyr::select_("bin", .dots = temp) %>%
    dplyr::filter(bin <= xmax) %>%
    tidyr::gather("key", "prob", -bin, factor_key = TRUE) %>%
    dplyr::mutate(profile = factor(stringr::str_extract(key, "[A-Z]+"),
                                   levels = c("OYP", "OFP", "ORP")),
                  max_pct = stringr::str_extract(key, "[0-9]+")) %>%
    ggplot2::ggplot(ggplot2::aes(x = bin, y = prob, linetype = max_pct)) +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous("Count", limits = c(0, xmax), labels = scales::comma) +
    ggplot2::scale_y_continuous("Probability", breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
    ggplot2::scale_linetype_discrete(name = "Percent of Max.")+
    ggplot2::facet_grid(profile ~ ., labeller = profile_label) +
    ggplot2::ggtitle(paste0(unique(profile_dat$stock_name), ": ", unique(profile_dat$trib_name))) +
    ggplot2::theme_bw()
  
  if(!anyNA(goal_range)) {
    plot + ggplot2::geom_rect(ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                              data.frame(xmin = goal_range[1], xmax = min(xmax, goal_range[2]), ymin = -Inf, ymax = Inf),
                              inherit.aes = FALSE, fill = "red", alpha = 0.2)
  }
  else plot
}


#' Expected sustained yield plot
#'
#' Expected sustained yield plot with 50 percent confidence ribbon
#'
#' @param profile_dat Output of the profile data function
#' @param limit Upper bounds for plot c(xmax, ymax). Default (NULL) will pick bounds from the data.
#' @param rug Show scaled statewide goal ranges. Defaults to TRUE.
#' @param goal_range A vector with two element c(lower_bound, upper_bound). Defaults to NA.
#'
#' @return A figure
#'
#' @examples
#' get_ids()
#' plot_ey(get_profile(post, stock_id[1]), c(5000, 9000))
#' profiles <- lapply(stock_id, get_profile, post_dat = post)
#' lapply(profiles, plot_ey)
#'
#' @export
plot_ey <- function(profile_dat, limit = NULL, rug = TRUE, goal_range = NA){
  rug_dat <- get_BEGbounds(median(profile_dat$S.msy))
  
  plot_dat <- profile_dat %>%
    dplyr::select(s, dplyr::starts_with("SY")) %>%
    dplyr::group_by(s) %>%
    dplyr::summarise(median.SY = median(SY, na.rm = TRUE),
                     p25.SY = quantile(SY, probs = 0.25, na.rm = TRUE),
                     p75.SY = quantile(SY, probs = 0.75, na.rm = TRUE)
    ) %>%
    tidyr::gather(Productivity, SY, median.SY)
  
  if(is.null(limit)){
    ymax <- max(plot_dat$p75.SY) * 1.05
    xmax <- plot_dat$s[which(plot_dat$p75.SY < 0)[1]]
    if(is.na(xmax)) 
      stop("Error: profile does not extend to escapements with zero yield, use a larger s_ub in get_profile()")
  }
  else {xmax <- limit[1]; ymax <- limit[2]}
  
  plot <-
    ggplot2::ggplot(plot_dat, ggplot2::aes(x = s, y = SY, color = Productivity)) +
    ggplot2::geom_line() +
    ggplot2::geom_ribbon(ggplot2::aes(x = s, ymin = p25.SY, ymax = p75.SY), inherit.aes = FALSE, alpha = 0.1) +
    ggplot2::scale_x_continuous("Spawners", labels = scales::comma) +
    ggplot2::scale_y_continuous("Expected Yield", labels = scales::comma) +
    ggplot2::coord_cartesian(xlim = c(0, xmax), ylim = c(0, ymax)) +
    ggplot2::scale_color_manual(name = "Productivity", labels = "1973-2013 broods", values = "black") +
    ggplot2::theme_bw()
  
  if(rug == TRUE) {
    plot2 <- plot +
      ggplot2::geom_rug(ggplot2::aes(x = lb), data = rug_dat, inherit.aes = FALSE, sides = "b", color = "darkgrey") +
      ggplot2::geom_rug(ggplot2::aes(x = ub), data = rug_dat, inherit.aes = FALSE, sides = "b", color = "black")
  }
  else plot2 <- plot
  
  if(!anyNA(goal_range)) {
    plot2 + ggplot2::geom_rect(ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                               data.frame(xmin = goal_range[1], xmax = goal_range[2], ymin = -Inf, ymax = Inf),
                               inherit.aes = FALSE, fill = "red", alpha = 0.2)
  }
  else plot2
  
}


#' Model fit plots
#'
#' Produces a faceted plot of Escapement and Inriver Run with the appropriately scaled indices of abundance that were used as inputs to the model.
#'
#' @param stats_dat The output from get_summary() for the SRA model mcmc.list output
#' @param stock_name A character element specifying the stock to plot.
#'
#' @return A figure
#'
#' @examples
#' get_ids()
#' plot_fit(get_summary(post), stock_id[1])
#' plot_fit(get_summary(post), "East Susitna")
#' lapply(stock_id, plot_fit, stats_dat = get_summary(post))
#'
#' @export
plot_fit <- function(stats_dat, stock_name){
  stopifnot(exists("year_id", .GlobalEnv),
            exists("age_max", .GlobalEnv),
            exists("stock_id", .GlobalEnv),
            "package:SusitnaEG" %in% search())
  yr0 <- as.numeric(min(year_id)) - 1
  yr0_R <- yr0 - age_max
  
  id0 <- lapply(1:5, function(x) colnames(as[[x]]))
  names(id0) <- names(as)
  id <- data.frame(stock = factor(rep(names(id0), sapply(id0, length)), levels = stock_id), 
                   tribn = unlist(sapply(id0, function(x) 1:length(x)), use.names = FALSE), 
                   tribn2 = 1:length(unlist(id0)),
                   trib = unlist(id0, use.names = FALSE),
                   stringsAsFactors = FALSE) %>%
    dplyr::arrange(stock, tribn, trib)
  
  trib <- function(node){
    temp <- stats_dat %>%
      tibble::rownames_to_column() %>%
      dplyr::filter(grepl(paste0("p.S", node, "\\["), rowname)) %>%
      dplyr::mutate(year = as.numeric(gsub("^.*\\[(\\d+).*$", "\\1", rowname)) + yr0,
                    tribn = as.numeric(gsub("^.*,(\\d)]$", "\\1", rowname)),
                    stock = stock_id[[node]],
                    node = node) %>%
      dplyr::filter(tribn != dim(as[[stock_name]])[2] + 1) %>%
      dplyr::select(year, stock, tribn, ps = Mean)
    if(node == 1) data.frame(year = as.numeric(year_id), stock = stock_id[[node]], tribn = 1, ps = 1) else temp
  } 
  
  theta <- 
    stats_dat %>%
    tibble::rownames_to_column() %>%
    dplyr::filter(grepl("^theta\\[", rowname)) %>%
    dplyr::mutate(tribn2 = as.numeric(gsub("theta\\[(\\d+),\\d+\\]$", "\\1", rowname)),
                  year = as.numeric(gsub("theta\\[\\d+,(\\d+)\\]$", "\\1", rowname)) + yr0) %>%
    dplyr::left_join(id, by = "tribn2") %>%
    dplyr::select(year, stock, tribn, theta = Mean)
  
  expand <- 
    lapply(1:5, function(x) trib(x)) %>% 
    do.call(rbind, .) %>%
    dplyr::left_join(theta, by = c("year", "stock", "tribn")) %>% 
    dplyr::mutate(ex_weir = ps,
                  ex_as = ex_weir * theta, 
                  year = as.character(year)) %>%
    dplyr::left_join(id, by = c("stock", "tribn")) %>%
    dplyr::select(year, stock, trib, ex_weir, ex_as)
  
  weirs <- weir %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "year") %>%
    dplyr::mutate(year = year_id[year]) %>%
    tidyr::gather(trib, count, -year) %>%
    dplyr::left_join(expand, by = c("year", "trib")) %>% 
    dplyr::mutate(value = count / ex_weir,
                  name = trib,
                  type = "weir",
                  name_f = "S") %>%
    dplyr::select(year, name, stock, type, name_f, value)
  
  surveys <- lapply(1:5, function(x) data.frame(stock = factor(unname(stock_id[x]), levels = stock_id),
                                                year = rep(unname(year_id), times = dim(as[[stock_id[x]]])[2]),
                                                trib = rep(colnames(as[[stock_id[x]]]), each = dim(as[[stock_id[x]]])[1]), 
                                                count = as[[stock_id[x]]] %>% as.list() %>% do.call(rbind, .),
                                                stringsAsFactors = FALSE)) %>%
    do.call(rbind, .) %>%
    dplyr::left_join(expand, by = c("year", "stock", "trib")) %>%
    dplyr::mutate(value = count / ex_as,
                  name = trib,
                  type ="survey",
                  name_f = "S") %>%
    dplyr::select(year, stock, name, type, name_f, value)
  
  markrecap <- 
    mr[[1]] %>%
    as.data.frame() %>%
    dplyr::mutate(year = year_id) %>%
    tidyr::gather(stock, value, -year) %>%
    dplyr::mutate(name = "Mark-Recapture",
                  type = "Mark-Recapture",
                  name_f = "IR") %>%
    dplyr::select(year, stock, name, type, name_f, value)
  
  indicies <- 
    rbind(weirs, surveys, markrecap) %>%
    dplyr::mutate(name_f = factor(name_f,
                                  levels = c("S", "IR"),
                                  labels = c("Escapement", "Inriver Run")),
                  type = factor(type, levels = c("survey", "weir", "Mark-Recapture")),
                  year = as.numeric(year)) %>%
    dplyr::filter(!is.na(value))
  
  pal <- RColorBrewer::brewer.pal(6, "Paired")
  breaks <- 
    id %>% 
    dplyr::select(-tribn2) %>%
    dplyr::mutate(name = factor(trib, levels = c(trib, "Mark-Recapture")),
                  color = unlist(lapply(sapply(1:5, function(x) sum(stock == stock_id[x])), function(x) pal[1:x]))) %>%
    dplyr::select(-tribn, - trib) %>%
    dplyr::filter(stock == stock_name) %>%
    rbind(data.frame(name = "Mark-Recapture",
                     stock = stock_name,
                     color = "black",
                     stringsAsFactors = FALSE)) %>%
    dplyr::arrange(stock, name, color)
  col <-setNames(breaks$color, breaks$name)
  sha <- c("survey" = 17, "weir" = 15, "Mark-Recapture" = 19)
  
  stats_dat %>%
    dplyr::select_(value = as.name("50%"), lcb = as.name("2.5%"), ucb = as.name("97.5%")) %>%
    tibble::rownames_to_column() %>%
    dplyr::filter(grepl("^S\\[|^IR\\[", rowname)) %>%
    dplyr::mutate(name_f = factor(gsub("(^.*)\\[.*", "\\1", rowname),
                                  levels = c("S", "IR"),
                                  labels = c("Escapement", "Inriver Run")),
                  stock = stock_id[gsub(".*,(\\d)\\]", "\\1", rowname)],
                  year = as.numeric(year_id[gsub(".*\\[(\\d+),.*", "\\1", rowname)])) %>%
    dplyr::filter(stock == stock_name) %>%
    ggplot2::ggplot(ggplot2::aes(x = year, y = value)) +
    ggplot2::geom_line() +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lcb, ymax = ucb), inherit.aes = TRUE, alpha = 0.3) +
    ggplot2::facet_grid(name_f ~ ., scales = "free_y", switch = "y") +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::geom_jitter(data = indicies[indicies$stock == stock_name, ], ggplot2::aes(color = name, shape = type), size = 3, width = .3) +
    #ggplot2::geom_pointrange(data = indicies2, ggplot2::aes(ymin = lb, ymax = ub, color = "IR.hat", shape = "IR.hat")) +
    ggplot2::scale_color_manual(name ="Index",
                                breaks = breaks$name,
                                values = col) +
    ggplot2::scale_shape_manual(name ="Index",
                                breaks = names(sha),
                                values = sha) +
    ggplot2::scale_x_continuous("Year", breaks = seq(min(year_id), max(year_id), 3), minor_breaks = NULL) +
    ggplot2::scale_y_continuous(minor_breaks = NULL, labels = scales::comma) +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(stock_name) +
    ggplot2::theme(strip.background = ggplot2::element_rect(colour="white", fill="white"), strip.placement = "outside")
}


#' Horsetail plot of plausible spawn-recruit relationships
#'
#' Produces a horsetail plot of the median Spawn-Recruit relationship.  Plot also shows 40 plausible Spawn-Recruit relationships in the background and Spawner and Recruit estimates with associated 90% CIs.
#'
#' @param post_dat SRA model mcmc.list output
#' @param stats_dat The output from get_summary() for the SRA model mcmc.list output
#' @param stock_name A character element specifying the stock to plot.
#'
#' @return A figure
#'
#' @examples
#' get_ids()
#' plot_horse(post, get_summary(post), stock_id[1])
#' plot_horse(post, get_summary(post), "East Susitna")
#' lapply(stock_id, plot_horse, post_dat = post, stats_dat = summary)
#'
#' @export
plot_horse <- function(post_dat, stats_dat, stock_name){
  stopifnot(exists("year_id", .GlobalEnv),
            exists("stock_id", .GlobalEnv),
            exists("age_max", .GlobalEnv))
  yr0 <- as.numeric(min(year_id)) - 1
  yr0_R <- yr0 - age_max
  
  stock_n <- which(stock_id == stock_name)
  coeflines <- sapply(paste0(c("beta", "lnalpha"), "[", stock_n, "]"), function(x){get_post(post_dat, var = x)}) %>% 
    as.data.frame() %>%
    dplyr::sample_n(40) %>%
    as.matrix() %>%
    plyr::alply(1, function(coef) {ggplot2::stat_function(fun=function(x){x * exp(coef[2] - coef[1] * x)}, colour="grey", alpha = 0.5)})
  
  param_50 <- stats_dat %>%
    tibble::rownames_to_column() %>%
    dplyr::filter(rowname %in% paste0(c("beta", "lnalpha"), "[", stock_n, "]")) %>%
    dplyr::select_(as.name("50%")) %>%
    unlist()
  
  temp <- stats_dat %>%
    dplyr::select_(median =as.name("50%"), lb = as.name("5%"), ub = as.name("95%")) %>%
    tibble::rownames_to_column() %>%
    dplyr::filter(grepl(paste0("^R\\[\\d+,", stock_n, "\\]|S\\[\\d+,", stock_n, "\\]"), rowname)) %>%
    dplyr::mutate(name = stringr::str_sub(rowname, 1, stringr::str_locate(rowname, "\\[")[, 1] - 1),
                  index = as.numeric(stringr::str_sub(rowname, stringr::str_locate(rowname, "[0-9]+"))),
                  year = (name != "R") * (yr0 + index) + (name == "R") * (yr0_R + index))
  
  v_dat <-  temp %>% dplyr::filter(name == "R") %>% dplyr::select(vlb = lb, vub = ub, year)
  h_dat <-  temp %>% dplyr::filter(name == "S") %>% dplyr::select(hlb = lb, hub = ub, year)
  upper = max(quantile(c(v_dat$vub, h_dat$hub), 0.95), temp$median)
  text_dat <-  temp %>%
    dplyr::select(median, name, year) %>%
    tidyr::spread(name, median) %>%
    dplyr::filter(!is.na(R) & ! is.na(S)) %>%
    dplyr::inner_join(v_dat, by = "year") %>%
    dplyr::inner_join(h_dat, by = "year")
  
  ggplot2::ggplot(text_dat, ggplot2::aes(x = S, y = R, label = year, ymin = vlb, ymax = vub, xmin = hlb, xmax = hub)) +
    ggplot2::geom_text() +
    ggplot2::geom_errorbar(linetype = 2) +
    ggplot2::geom_errorbarh(linetype = 2) +
    ggplot2::stat_function(fun=function(x){x * exp(param_50[2] - param_50[1] * x)}, size = 2, linetype = 2) +
    coeflines +
    ggplot2::scale_x_continuous("Spawners", limits = c(0, upper), minor_breaks = NULL, labels = scales::comma) +
    ggplot2::scale_y_continuous("Recruits", minor_breaks = NULL, labels = scales::comma) +
    ggplot2::coord_cartesian(xlim = c(0, upper), ylim = c(0, upper)) +
    ggplot2::geom_abline(slope = 1, size = 1) +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(stock_name) +
    ggplot2::theme(strip.background = ggplot2::element_rect(colour="white", fill="white"))
}


#' OYP, ORP and/or OFP plots
#'
#' Produces a faceted plot of OYP, ORP or OFP with an overlay of the proposed goal range and a rug showing appropriately scaled upper and lower bounds of other statewide goals.
#'
#' @param profile_dat Output of the get_profile function
#' @param limit Upper bound of spawners for plot. Default (NULL) will use 2.25 times S.msy.
#' @param rug Show scaled statewide goal ranges. Defaults to TRUE.
#' @param goal_range A vector with two element c(lower_bound, upper_bound). Defaults to NA.
#' @param profiles the profiles to plot as a character vector e.g. c("OYP", "OFP").  Defaults to c("OYP", "ORP", "OFP").
#'
#' @return A figure
#'
#' @examples
#' plot_profile(get_profile(post, "Deshka"), rug = FALSE, NA, c("OYP", "OFP"))
#'
#' @export
plot_profile <- function(profile_dat, limit = NULL, rug = TRUE, goal_range = NA, profiles = c("OYP", "ORP", "OFP")){
  temp <-unlist(lapply(profiles, function(x){paste0(x, c("70", "80", "90"))}))
  profile_label <- ggplot2::as_labeller(c('OYP' = "Optimum Yield Profile",
                                          'OFP' = "Overfishing Profile",
                                          'ORP' = "Optimum Recruitment Profile"))
  S.msy50 <- median(profile_dat$S.msy) 
  rug_dat <- get_BEGbounds(S.msy50)
  
  if(is.null(limit)){
    xmax <- S.msy50 * 2.25
  }
  else xmax <- limit
  
  plot <- profile_dat %>%
    dplyr::select_("s", .dots = temp) %>%
    dplyr::group_by(s) %>%
    dplyr::filter(s <= xmax) %>%
    dplyr::summarise_all(mean, na.rm = TRUE) %>%
    tidyr::gather("key", "prob", -s, factor_key = TRUE) %>%
    dplyr::mutate(profile = factor(stringr::str_extract(key, "[A-Z]+"),
                                   levels = c("OYP", "OFP", "ORP")),
                  max_pct = stringr::str_extract(key, "[0-9]+")) %>%
    ggplot2::ggplot(ggplot2::aes(x = s, y = prob, linetype = max_pct)) +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous("Spawners", limits = c(0, xmax), labels = scales::comma) +
    ggplot2::scale_y_continuous("Probability", breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
    ggplot2::scale_linetype_discrete(name = "Percent of Max.")+
    ggplot2::facet_grid(profile ~ ., labeller = profile_label) +
    ggplot2::theme_bw()
  
  if(rug == TRUE) {
    plot2 <- plot +     
      ggplot2::geom_rug(ggplot2::aes(x = lb), data = rug_dat, inherit.aes = FALSE, sides = "b", color = "darkgrey") +
      ggplot2::geom_rug(ggplot2::aes(x = ub), data = rug_dat, inherit.aes = FALSE, sides = "b", color = "black")
  }
  else plot2 <- plot
  
  if(!anyNA(goal_range)) {
    plot2 + ggplot2::geom_rect(ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                               data.frame(xmin = goal_range[1], xmax = goal_range[2], ymin = -Inf, ymax = Inf),
                               inherit.aes = FALSE, fill = "red", alpha = 0.2)
  }
  else plot2
}


#' Ricker Curves with annual productivity
#'
#' logRhat.y <- log(S.y) + lnalpha - beta * S.y 
#' logresid.y <- log(R.y) - logRhat.y
#' lnalpha.y <- lnalpha + logresid.y
#' 
#' @param stats_dat The output from get_summary() for the SRA model mcmc.list output
#' @param stock_name A character element specifying the stock to plot.
#'
#' @return A figure
#'
#' @examples
#' get_ids()
#' plot_rickeryear(get_summary(post), stock_id[1])
#' plot_rickeryear(get_summary(post), "East Susitna")
#' lapply(stock_id, plot_rickeryear, stats_dat = get_summary(post))
#'
#' @export
plot_rickeryear <- function(stats_dat, stock_name){
  stopifnot(exists("year_id", .GlobalEnv),
            exists("stock_id", .GlobalEnv),
            exists("age_max", .GlobalEnv))
  yr0 <- as.numeric(min(year_id)) - 1
  yr0_R <- yr0 - age_max
  
  stock_n <- which(stock_id == stock_name)
  
  text <- stats_dat %>%
    dplyr::select_(median =as.name("50%")) %>%
    tibble::rownames_to_column() %>%
    dplyr::filter(grepl(paste0("^R\\[\\d+,", stock_n, "\\]|S\\[\\d+,", stock_n, "\\]"), rowname)) %>%
    dplyr::mutate(name = stringr::str_sub(rowname, 1, stringr::str_locate(rowname, "\\[")[, 1] - 1),
                  index = as.numeric(stringr::str_sub(rowname, stringr::str_locate(rowname, "[0-9]+"))),
                  year = (name != "R") * (yr0 + index) + (name == "R") * (yr0_R + index)) %>%
    dplyr::select(median, name, year) %>%
    tidyr::spread(name, median) %>%
    dplyr::filter(!is.na(R) & ! is.na(S))
  
  lines <- stats_dat %>%
    dplyr::select(value = Mean) %>%
    tibble::rownames_to_column()  %>% 
    dplyr::filter(grepl(paste0("^lnalpha.vec\\[\\d+,", stock_n, "\\]"), rowname)) %>%
    dplyr::mutate(beta = as.numeric(stats_dat[grepl(paste0("beta\\[", stock_n, "\\]"), rownames(stats_dat)), "Mean"])) %>%
    dplyr::select(value, beta) %>%
    as.matrix() %>%
    plyr::alply(1, function(coef) {ggplot2::stat_function(fun = function(x){x * exp(coef[1] - coef[2] * x)}, colour="grey", alpha = 0.5)})
  
  upper = max(text[, c("R", "S")]) * 1.1
  
  ggplot2::ggplot(text, ggplot2::aes(x = S, y = R, label = year)) +
    ggplot2::geom_text() +
    lines +
    ggplot2::scale_x_continuous("Spawners", limits = c(0, upper), minor_breaks = NULL, labels = scales::comma) +
    ggplot2::scale_y_continuous("Recruits", limits = c(0, NA), minor_breaks = NULL, labels = scales::comma) +
    ggplot2::coord_cartesian(xlim = c(0, upper), ylim = c(0, upper)) +
    ggplot2::theme_bw() +
    ggplot2::theme(strip.background = ggplot2::element_rect(colour="white", fill="white"))
}


#' State Variable Plot
#'
#' Produces a faceted plot of escapement, recruitment, total run, Ricker residuals and harvest rate plotted with 95% confidence envelopes.
#'
#' @param stats_dat The output from get_summary() for the SRA model mcmc.list output
#' @param stock_name A character element specifying the stock to plot.
#' @param S_msr Logical (TRUE) indicating if S_msr shoud be included in the escapement panel.  Defaults to FALSE.
#'
#' @return A figure
#'
#' @examples
#' plot_state(get_summary(post), stock_id[1])
#' plot_state(get_summary(post), "East Susitna")
#' lapply(stock_id, plot_state, stats_dat = get_summary(post))
#'
#' @export
plot_state <- function(stats_dat, stock_name, S_msr = FALSE){
  stopifnot(exists("year_id", .GlobalEnv),
            exists("age_max", .GlobalEnv),
            exists("stock_id", .GlobalEnv))
  yr0 <- as.numeric(min(year_id)) - 1
  yr0_R <- yr0 - age_max
  stock <- unname(which(stock_id == stock_name))
  
  msy50 <- stats_dat %>%
    dplyr::select_(median = as.name("50%")) %>%
    tibble::rownames_to_column() %>%
    dplyr::filter(grepl(paste0("msy\\[", stock, "\\]"), rowname)) %>%
    dplyr::mutate(name = factor(stringr::str_sub(rowname, stringr::str_locate(rowname, ".")),
                                levels = c("S", "U"),
                                labels = c("Escapement", "Harvest Rate")))
  
  msr50 <- stats_dat %>%
    dplyr::select_(median = as.name("50%")) %>%
    tibble::rownames_to_column() %>%
    dplyr::filter(grepl(paste0("beta\\[", stock, "\\]|lnalpha\\[", stock, "\\]"), rowname)) %>%
    dplyr::mutate(msr = ifelse(rowname == paste0("beta[", stock, "]"), 1 / median, 1-1/exp(median)),
                  name = factor(c("S", "U"),
                                levels = c("S", "U"),
                                labels = c("Escapement", "Harvest Rate")))
  
  plot <-
    stats_dat %>%
    dplyr::select_(median = as.name("50%"), lcb = as.name("2.5%"), ucb = as.name("97.5%")) %>%
    tibble::rownames_to_column() %>%
    dplyr::filter(grepl(paste0("^R\\[\\d+,", stock, 
                               "\\]|S\\[\\d+,", stock, 
                               "\\]|N\\[\\d+,", stock, 
                               "\\]|log.resid.vec\\[\\d+,", stock, 
                               "\\]|mu.Habove\\[\\d+,", stock, "\\]"), rowname)) %>%
    dplyr::mutate(name = factor(stringr::str_sub(rowname, 1, stringr::str_locate(rowname, "\\[")[, 1] - 1),
                                levels = c("S", "N", "R", "mu.Habove", "log.resid.vec"),
                                labels = c("Escapement", "Total Run", "Recruitment", "Harvest Rate", "Ricker Residuals")),
                  index = as.numeric(stringr::str_sub(rowname, stringr::str_locate(rowname, "[0-9]+"))),
                  year = (name != c("Recruitment")) * (yr0 + index) +
                    (name == "Recruitment") * (yr0_R + index)) %>%
    dplyr::filter(year >= yr0) %>%
    ggplot2::ggplot(ggplot2::aes(x = year, y = median)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lcb, ymax = ucb), inherit.aes = TRUE, alpha = 0.3) +
    ggplot2::facet_grid(name ~ ., scales = "free_y", switch = "y") +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::scale_x_continuous("Year", breaks = seq(min(year_id), max(year_id), 3), minor_breaks = NULL)  +
    ggplot2::scale_y_continuous(minor_breaks = NULL, labels = scales::comma)  +
    ggplot2::geom_hline(data = msy50, ggplot2::aes(yintercept = median), color = "red", linetype = 2) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = 0), color = "black", linetype = 1) +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(stock_name) +
    ggplot2::theme(strip.background = ggplot2::element_rect(colour="white", fill="white"), strip.placement = "outside")
  
  if(S_msr == TRUE) {plot <- plot +     ggplot2::geom_hline(data = msr50, ggplot2::aes(yintercept = msr), color = "red", linetype = 5)}
  
  plot
}


#' Ricker Posterior correlations plot
#'
#' Pairs plot for beta, ln(alpha), phi, S.msy and sigma.white
#'
#' @param dat_post posterior object
#' @param pars nodes to plot. Defaults to beta, ln(alpha), phi, S.msy and sigma.white.
#' @param n number of pairs to plot. Defaults to 200.
#' @param trim percentage of extreme S.msy realizations to remove. Defaults to 0.05.
#'
#' @return A figure
#'
#' @examples
#' plot_statepairs(post)
#'
#' @export
plot_statepairs <- function(dat_post, 
                            pars = c(paste0("beta[", 1:5, "]"),
                                     paste0("lnalpha[", 1:5, "]"),
                                     paste0("phi[", 1:5, "]"),
                                     paste0("sigma.white[", 1:5, "]")), 
                            n = 200, 
                            trim = 0.05){
  postdf <- as.data.frame(as.matrix(dat_post))   
  bounds <- lapply(postdf[, grepl("S.msy", names(postdf))], quantile, probs = c(trim / 2, 1 - trim / 2))
  index <- Reduce(intersect, lapply(1:5, function(x) which(postdf[names(bounds)][, x] > bounds[[x]][1] & postdf[names(bounds)][, x] < bounds[[x]][2])))
  subset <- postdf[index, pars]
  pairs(subset[sample(1:dim(subset)[1], n), ], cex=0.6)
}


#' Plots of composition by stock
#'
#' Faceted plot of stock composition for Susitna Drainage. (Need to add observations)
#'
#' @param input_dat telemetry data matrix
#' @param stats_dat The output from get_summary() for the SRA model mcmc.list ouput
#'
#' @return A figure
#'
#' @examples
#' plot_stock(telemetry, get_summary(post))
#'
#' @export
plot_stock <- function(input_dat, stats_dat){
  stopifnot(exists("year_id", .GlobalEnv),
            exists("age_max", .GlobalEnv),
            exists("stock_id", .GlobalEnv),
            "package:SusitnaEG" %in% search())
  yr0 <- as.numeric(min(year_id)) - 1
  yr0_R <- yr0 - age_max
  
  id0 <- lapply(1:5, function(x) c(colnames(as[[x]]), paste0("Other ", names(as[x]))))
  names(id0) <- names(as)
  id <- data.frame(stock = factor(rep(names(id0), sapply(id0, length)), levels = stock_id), 
                   tribn = unlist(sapply(id0, function(x) 1:length(x)), use.names = FALSE), 
                   trib0 = unlist(id0, use.names = FALSE),
                   stringsAsFactors = FALSE) %>%
    dplyr::mutate(trib = factor(trib0, levels = trib0)) %>%
    dplyr::select(-trib0) %>%
    dplyr::arrange(stock, tribn)
  
  obs_f <- function(stock){
    input_dat[[stock]] %>%
      (function(x) {x/rowSums(x)}) %>%
      as.data.frame() %>%
      tibble::rownames_to_column() %>%
      tidyr::gather(trib0, p0, -rowname) %>%
      dplyr::mutate(trib = factor(ifelse(trib0 == "Other", paste0(trib0, " ", stock), trib0), 
                                  levels = id$trib[id$stock == stock]),
                    year = unname(year_id[rowname]),
                    stock = stock) %>%
      dplyr::filter(!is.na(p0)) %>%
      dplyr::arrange(year, dplyr::desc(trib)) %>%
      dplyr::group_by(year) %>%
      dplyr::mutate(p = cumsum(p0)) %>%
      dplyr::ungroup() %>%
      dplyr::select(year, stock, trib, Mean = p)
  }
  
  obs <- 
    lapply(stock_id[-1], obs_f) %>% do.call(rbind, .) %>%
    dplyr::mutate(stock = factor(stock, levels = stock_id))
  
  est <- stats_dat %>%
    tibble::rownames_to_column() %>%
    dplyr::filter(grepl("p.S", rowname)) %>%
    dplyr::mutate(year = as.numeric(gsub("^.*\\[(\\d+).*$", "\\1", rowname)) + yr0,
                  stock = factor(unname(stock_id[gsub("p.S(\\d).*", "\\1", rowname)]), levels = stock_id),
                  tribn = as.numeric(gsub("^.*,(\\d)]$", "\\1", rowname))) %>%
    dplyr::left_join(id, by = c("stock", "tribn")) %>%
    dplyr::select(stock, year, trib, Mean) 
  
  pal <- RColorBrewer::brewer.pal(7, "Paired")
  breaks <- id[!id$stock == "Deshka", ] %>% 
    dplyr::mutate(color = unlist(lapply(sapply(2:5, function(x) sum(stock == stock_id[x]) - 1), function(x) c(pal[1:x], "black"))),
                  alpha = 1.25 - (as.numeric(stock) - 1)/length(stock_id[-1])) %>%
    dplyr::select(-tribn) %>%
    dplyr::arrange(stock, trib, color, alpha)
  col <-setNames(breaks$color, breaks$trib)
  alp <-setNames(breaks$alpha, breaks$trib)
  
  est %>%
    ggplot2::ggplot(ggplot2::aes(x = as.numeric(year), y = Mean, fill = trib, alpha = trib)) +
    ggplot2::geom_area() +
    ggplot2::facet_grid(stock ~ ., switch = "y") +
    ggplot2::scale_x_continuous(breaks = seq(min(year_id), max(year_id), 3), minor_breaks = NULL) +
    ggplot2::scale_y_continuous(minor_breaks = NULL, labels = scales::percent) +
    ggplot2::scale_fill_manual(breaks = breaks$trib, values = col) +
    ggplot2::scale_alpha_manual(breaks = breaks$trib, values = alp) +
    ggplot2::geom_point(data = obs, ggplot2::aes(fill = trib), size = 3, shape = 21, color = "white") +
    ggplot2::labs(y = NULL, x = "Year") +
    ggplot2::theme(strip.background = ggplot2::element_rect(colour="white", fill="white"), strip.placement = "outside")
  
}


#' Plots of Aerial Survey detectability
#'
#' Estimated and observed aerial survey detectability in the Susitna River drainage.
#'
#' @param stats_dat The output from get_summary() for the SRA model mcmc.list output
#'
#' @return A figure
#'
#' @examples
#' plot_theta(get_summary(post))
#'
#' @export
plot_theta <- function(stats_dat){
  stopifnot(exists("year_id", .GlobalEnv),
            exists("stock_id", .GlobalEnv),
            "package:SusitnaEG" %in% search())
  
  yr0 <- as.numeric(min(year_id)) - 1
  
  id0 <- lapply(1:5, function(x) colnames(as[[x]]))
  names(id0) <- names(as)
  id <- data.frame(stock = factor(rep(names(id0), sapply(id0, length)), levels = stock_id), 
                   tribn = unlist(sapply(id0, function(x) 1:length(x)), use.names = FALSE), 
                   tribn2 = 1:length(unlist(id0)),
                   trib = unlist(id0, use.names = FALSE))
  
  theta_est <- 
    tibble::rownames_to_column(stats_dat) %>% 
    dplyr::filter(grepl(paste0("^theta\\["), rowname)) %>% 
    dplyr::mutate(year = as.numeric(gsub("theta\\[\\d+,(\\d+)\\]", "\\1", rowname)) + yr0,
                  tribn2 = as.numeric(gsub("theta\\[(\\d+),\\d+\\]", "\\1", rowname))) %>%
    dplyr::left_join(id, by = "tribn2") %>%
    dplyr::select(year, stock, trib, theta = Mean)
  
  theta_obs1 <- data.frame(year = as.numeric(year_id), stock = unname(stock_id[1]), trib = "Deshka", theta = as[["Deshka"]][, "Deshka"] / weir[, "Deshka"])
  theta_obs2 <- data.frame(year = as.numeric(year_id), stock = unname(stock_id[2]), trib = "Montana", theta = as[["East Susitna"]][, "Montana"] / weir[, "Montana"])
  theta_obs3 <- data.frame(year = as.numeric(year_id), stock = unname(stock_id[2]), trib = "Willow", theta = as[["East Susitna"]][, "Willow"] / weir[, "Willow"])
  theta_obs <- 
    rbind(theta_obs1, theta_obs2, theta_obs3) %>%
    dplyr::filter(!is.na(theta))
  
  pal <- RColorBrewer::brewer.pal(6, "Paired")
  breaks <- 
    id %>% 
    dplyr::select(-tribn, -tribn2) %>%
    dplyr::mutate(color = unlist(lapply(sapply(1:5, function(x) sum(stock == stock_id[x])), function(x) pal[1:x]))) %>%
    dplyr::arrange(stock, trib, color)
  col <-setNames(breaks$color, breaks$trib)
  
  ggplot2::ggplot(theta_est, ggplot2::aes(x = year, y = theta, color = trib)) +
    ggplot2::geom_line(size = 2) +
    ggplot2::geom_point(data = theta_obs, size = 3) +
    ggplot2::facet_grid(. ~ stock) +
    ggplot2::scale_color_manual(name ="Trib",
                                breaks = breaks$trib,
                                values = col)
}
