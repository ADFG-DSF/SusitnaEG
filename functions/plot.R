#' Plots of composition and abundance by age
#'
#' Faceted plot of age at maturity, age composition and total run by age.  Observed age composition is also plotted.
#'
#' @param post_dat The posterior object from the SRA model of class jagsUI
#'
#' @return A figure
#'
#' @examples
#' plot_age(post)
#'
#' @export
plot_age <- function(post_dat){
  stopifnot(exists("year_id", .GlobalEnv),
            exists("age_max", .GlobalEnv))
  yr0 <- as.numeric(min(year_id)) - 1
  yr0_p <- yr0 - age_max
  
Q.obs <- 
  as.data.frame(post_dat$data$x.a / rowSums(post_dat$data$x.a)) %>%
  setNames(paste0("age", 1:ncol(post_dat$data$x.a))) %>%
  dplyr::mutate(sample = factor(post_dat$data$x.stock, levels = names(stock_id), labels = stock_id)) %>%
  tibble::rownames_to_column(var = "yr_id") %>%
  tidyr::gather(age, prop, dplyr::starts_with("age")) %>%
  dplyr::group_by(yr_id) %>%
  dplyr::mutate(mean = cumsum(prop), plot = "Age Composition") %>%
  dplyr::ungroup(yr_id) %>%
  dplyr::mutate(year = as.numeric(year_id[post_dat$data$yr.a[as.numeric(yr_id)]]))
                
P.mn <- get_array(post_dat, "p") %>%
  dplyr::mutate(plot = "Age-at-Maturity")
Q.mn <- get_array(post_dat, "q") %>%
  dplyr::mutate(plot = "Age Composition")
N.mn <- get_array(post_dat, "N.ta") %>%
  dplyr::mutate(plot = "Total Run")

pi.mn <- get_array(post_dat, "pi") %>%
  dplyr::group_by(yr) %>%
  dplyr::mutate(mean = cumsum(mean), 
                plot = "Age-at-Maturity",
                year = yr0_p + yr,
                age = paste0("age", age)) %>%
  dplyr::ungroup(year) %>%
  dplyr::filter(age != "age4")

dplyr::bind_rows(P.mn, Q.mn, N.mn) %>%
  dplyr::mutate(year = (plot != c("Age-at-Maturity")) * (yr0 + yr) + (plot == c("Age-at-Maturity")) * (yr0_p + yr),
                age = paste0("age", age)) %>%
  ggplot2::ggplot(ggplot2::aes(x = year, y = mean, alpha = age)) +
    ggplot2::geom_area(position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::facet_grid(plot ~ ., scales = "free", switch = "y") +
    ggplot2::scale_x_continuous(breaks = seq(yr0_p, max(year_id), 4), minor_breaks = NULL) +
    ggplot2::scale_y_continuous(minor_breaks = NULL, labels = digits) +
    ggplot2::geom_point(data = Q.obs, size = 3, ggplot2::aes(shape = sample)) +
    ggplot2::geom_line(data = pi.mn, ggplot2::aes(alpha = NULL, color = age), size = 0.75, linetype = "dashed") +
    ggplot2::scale_color_manual(values = rep("black", 4), guide = "none") +
    ggplot2::scale_alpha_discrete(name = "Age", labels = names(age_id), guide = ggplot2::guide_legend(reverse = TRUE)) +
    ggplot2::scale_shape_discrete(name = "Sample") +
    ggplot2::labs(y = NULL, x = "Year") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle("Susitna River Chinook salmon Age Composition") +
    ggplot2::theme(strip.background = ggplot2::element_rect(colour="white", fill="white"), 
                   strip.placement = "outside") +
    ggplot2::theme_bw(base_size = 14)
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
  stopifnot(exists("stock_id", .GlobalEnv))
  
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
    dplyr::mutate(profile = factor(gsub("([A-Z]+)[0-9]+", "\\1", key),
                                   levels = c("OYP", "OFP", "ORP")),
                  max_pct = gsub("[A-Z]+([0-9]+)", "\\1", key)) %>%
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
  stopifnot(exists("stock_id", .GlobalEnv),
            exists("stock_print", .GlobalEnv))
  rug_dat <- get_BEGbounds(median(profile_dat$S.msy))
  stock_name <- paste0(stock_print[which(stock_id == unique(profile_dat[["name"]]))], " Stock Expected Sustained Yield")
  
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
    ggplot2::scale_color_manual(guide = FALSE, values = "black") +
    ggplot2::ggtitle(stock_name) +
    ggplot2::theme_bw(base_size = 11)
  
  if(rug == TRUE) {
    plot2 <- plot +
      ggplot2::geom_rug(ggplot2::aes(x = lb), data = rug_dat, inherit.aes = FALSE, sides = "b", color = "darkgrey") +
      ggplot2::geom_rug(ggplot2::aes(x = ub), data = rug_dat, inherit.aes = FALSE, sides = "b", color = "black")
  }
  else plot2 <- plot
  
  if(!anyNA(goal_range)) {
    dat <- data.frame(xmin = unname(goal_range[1]), xmax = unname(goal_range[2]), ymin = -Inf, ymax = Inf)
    plot2 + ggplot2::geom_rect(ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                               data = dat,
                               inherit.aes = FALSE, fill = "red", alpha = 0.2)
  }
  else plot2
  
}


#' Model fit plots
#'
#' Produces a faceted plot of Escapement and Inriver Run with the appropriately scaled indices of abundance that were used as inputs to the model.
#'
#' @param post_dat The posterior object from the SRA model of class jagsUI
#' @param stock_name A character element specifying the stock to plot.
#'
#' @return A figure
#'
#' @examples
#' get_ids()
#' plot_fit(post, stock_id[1])
#' plot_fit(post, "East Susitna")
#' lapply(stock_id, plot_fit, post_dat = post)
#'
#' @export
plot_fit <- function(post_dat, stock_name){
  stopifnot(exists("year_id", .GlobalEnv),
            exists("age_max", .GlobalEnv),
            exists("stock_id", .GlobalEnv),
            exists("stock_print", .GlobalEnv),
            exists("trib_id", .GlobalEnv))
  yr0 <- as.numeric(min(year_id)) - 1
  yr0_R <- yr0 - age_max
  
  id0 <- lapply(trib_id, function(x){x[!grepl("Other", x)]})
  names(id0) <- names(as)
  id <- data.frame(stock = factor(rep(names(id0), sapply(id0, length)), levels = stock_id), 
                   tribn = unlist(sapply(id0, function(x) 1:length(x)), use.names = FALSE), 
                   tribn2 = 1:length(unlist(id0)),
                   trib = unlist(id0, use.names = FALSE),
                   stringsAsFactors = FALSE) %>%
    dplyr::arrange(stock, tribn, trib)
  
  trib <- function(node){
    temp <- 
      post_dat[["summary"]][grepl(paste0("p.S", node, "\\["), rownames(post_dat$summary)), c("mean", "sd")] %>%
      as.data.frame() %>%
      tibble::rownames_to_column() %>%
      dplyr::mutate(year = as.numeric(gsub("^.*\\[(\\d+).*$", "\\1", rowname)) + yr0,
                    tribn = as.numeric(gsub("^.*,(\\d)]$", "\\1", rowname)),
                    stock = stock_id[[node]],
                    node = node) %>%
      dplyr::filter(tribn != dim(as[[stock_name]])[2] + 1) %>%
      dplyr::select(year, stock, tribn, ps = mean)
    if(node == 1) data.frame(year = as.numeric(year_id), stock = stock_id[[node]], tribn = 1, ps = 1) else temp
  } 
  
  theta <- 
    post_dat[["summary"]] %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    dplyr::filter(grepl("^theta\\[", rowname)) %>%
    dplyr::mutate(tribn2 = as.numeric(gsub("theta\\[(\\d+),\\d+\\]$", "\\1", rowname)),
                  year = as.numeric(gsub("theta\\[\\d+,(\\d+)\\]$", "\\1", rowname)) + yr0) %>%
    dplyr::left_join(id, by = "tribn2") %>%
    dplyr::select(year, stock, tribn, theta = mean)
  
  expand <- 
    lapply(1:length(stock_id), function(x) trib(x)) %>% 
    do.call(rbind, .) %>%
    dplyr::left_join(theta, by = c("year", "stock", "tribn")) %>% 
    dplyr::mutate(ex_weir = ps,
                  ex_as = ex_weir * theta, 
                  year = as.character(year)) %>%
    dplyr::left_join(id, by = c("stock", "tribn")) %>%
    dplyr::select(year, stock, trib, ex_weir, ex_as)
  
  weirs <- 
    weir %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "year") %>%
    dplyr::mutate(year = year_id[year]) %>%
    tidyr::gather(trib, count, -year) %>%
    dplyr::left_join(expand, by = c("year", "trib")) %>% 
    dplyr::mutate(value = count / ex_weir,
                  name = paste0(trib, " weir"),
                  type = "weir",
                  name_f = "S") %>%
    dplyr::select(year, name, stock, name_f, trib, type, value)
  
  surveys <- lapply(1:length(stock_id), function(x) data.frame(stock = factor(unname(stock_id[x]), levels = stock_id),
                                                year = rep(unname(year_id), times = dim(as[[stock_id[x]]])[2]),
                                                trib = rep(colnames(as[[stock_id[x]]]), each = length(year_id)), 
                                                count = as[[stock_id[x]]][1:length(year_id), ] %>% as.list() %>% do.call(rbind, .),
                                                stringsAsFactors = FALSE)) %>%
    do.call(rbind, .) %>%
    dplyr::left_join(expand, by = c("year", "stock", "trib")) %>%
    dplyr::mutate(value = count / ex_as,
                  name = paste0(trib, " survey"),
                  type = "survey",
                  name_f = "S") %>%
    dplyr::select(year, stock, name, name_f, trib, type, value)
  
  sonar <- 
    data.frame(count = post_dat$data$sonar,
               prop = post_dat$mean$p.p2upS4) %>%
    tibble::rownames_to_column(var = "year") %>%
    dplyr::mutate(year = year_id[year],
                  trib = "Lake") %>%
    dplyr::left_join(expand, by = c("year", "trib")) %>% 
    dplyr::mutate(value = count / prop / ex_weir,
                  stock = "Yentna",
                  name = paste0(trib, " sonar"),
                  type = "sonar",
                  name_f = "S") %>%
    dplyr::select(year, name, stock, name_f, trib, type, value)
  
  markrecap <- 
    mr[[1]][1:length(year_id), ] %>%
    as.data.frame() %>%
    dplyr::mutate(year = year_id) %>%
    tidyr::gather(stock, value, -year) %>%
    dplyr::mutate(name = "Mark-Recapture",
                  name_f = "IR") %>%
    dplyr::select(year, stock, name, name_f, value)
  
  indicies <- 
    rbind(weirs, surveys) %>%
    rbind(sonar) %>%
    dplyr::mutate(name_f = factor(name_f,
                                  levels = c("S", "IR"),
                                  labels = c("Escapement", "Inriver Run")),
                  year = as.numeric(year)) %>%
    dplyr::filter(!is.na(value))
  
  indicies2 <-
    dplyr::left_join(post_dat[["data"]][["MR"]] %>% 
                       as.data.frame() %>%
                       tibble::rownames_to_column(var = "yr") %>%
                       tidyr::gather(stock, value, -yr),
                     post_dat[["data"]][["cv.MR"]] %>% 
                       as.data.frame() %>%
                       tibble::rownames_to_column(var = "yr") %>%
                       tidyr::gather(stock, cv, -yr), 
                     by = c("yr", "stock")) %>%
    dplyr::mutate(year = yr0 + as.numeric(yr),
                  name = "Mark-Recapture",
                  name_f = "Inriver Run",
                  ub = exp(log(value) + 1.96 * sqrt(log(cv * cv + 1))),
                  lb = exp(log(value) - 1.96 * sqrt(log(cv * cv + 1)))) %>%
    dplyr::filter(!is.na(value))
  
  pal <- RColorBrewer::brewer.pal(6, "Set1")
  shapes <- c("survey" = 17, "weir" = 15, "Mark-Recapture" = 19, sonar = 18)
  breaks <- 
    indicies[!duplicated(indicies$name), c("stock", "name", "trib", "type")] %>%
    dplyr::left_join(id[, c("trib", "tribn")], by = "trib") %>%
    dplyr::mutate(name = as.factor(name),
                  color = unlist(pal[tribn])) %>%
    dplyr::select(-trib, -tribn) %>%
    dplyr::filter(stock == stock_name) %>%
    rbind(data.frame(name = "Mark-Recapture",
                     type = "Mark-Recapture",
                     stock = stock_name,
                     color = "black",
                     stringsAsFactors = FALSE)) %>%
    dplyr::mutate(shape = shapes[type]) %>%
    dplyr::arrange(stock, name, color)
  col <-setNames(breaks$color, breaks$name)
  sha <- setNames(breaks$shape, breaks$name)
  
  post_dat$summary %>%
    as.data.frame() %>%
    dplyr::select(value = "50%", lcb = "2.5%", ucb = "97.5%") %>%
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
    ggplot2::geom_jitter(data = indicies[indicies$stock == stock_name, ], ggplot2::aes(color = name, shape = name), size = 3, width = .3) +
    ggplot2::geom_pointrange(data = indicies2[indicies2$stock == stock_name, ], 
                             ggplot2::aes(ymin = lb, ymax = ub, color = "Mark-Recapture", shape = "Mark-Recapture"),
                             show.legend = FALSE) +
    ggplot2::scale_color_manual(name ="Index",
                                breaks = breaks$name,
                                values = col,
                                guide = ggplot2::guide_legend(ncol = 4)) +
    ggplot2::scale_shape_manual(name ="Index",
                                breaks = breaks$name,
                                values = sha,
                                guide = ggplot2::guide_legend(ncol = 4)) +
    ggplot2::scale_x_continuous("Year", breaks = seq(min(year_id), max(year_id), 3), minor_breaks = NULL) +
    ggplot2::scale_y_continuous(minor_breaks = NULL, labels = scales::comma) +
    ggplot2::theme_bw(base_size = 17) +
    ggplot2::ggtitle(paste0(stock_print[which(stock_id == stock_name)], " Stock Escapement and Inriver Run")) +
    ggplot2::theme(strip.background = ggplot2::element_rect(colour="white", fill="white"),
                   legend.position = "bottom",
                   strip.placement = "outside")
}


#' Horsetail plot of plausible spawn-recruit relationships
#'
#' Produces a horsetail plot of the mean Spawn-Recruit relationship.  Plot also shows 40 plausible Spawn-Recruit relationships in the background and Spawner and Recruit estimates with associated 90% CIs.
#'
#' @param post_dat SRA model mcmc.list output
#' @param stock_name A character element specifying the stock to plot.
#'
#' @return A figure
#'
#' @examples
#' get_ids()
#' plot_horse(post, stock_id[1])
#' plot_horse(post, "East Susitna")
#' lapply(stock_id, plot_horse, post_dat = post)
#'
#' @export
plot_horse <- function(post_dat, stock_name){
  stopifnot(exists("year_id", .GlobalEnv),
            exists("stock_id", .GlobalEnv),
            exists("stock_print", .GlobalEnv),
            exists("age_max", .GlobalEnv))
  yr0 <- as.numeric(min(year_id)) - 1
  yr0_R <- yr0 - age_max
  
  stock_n <- which(stock_id == stock_name)
  coeflines <- 
    data.frame(beta = post_dat$sims.list[["beta"]][, stock_n], lnalpha = post_dat$sims.list[["lnalpha.c"]][, stock_n]) %>%
    dplyr::sample_n(40) %>%
    as.matrix() %>%
    plyr::alply(1, function(coef) {ggplot2::stat_function(fun=function(x){x * exp(coef[2] - coef[1] * x)}, colour="grey", alpha = 0.5)})
  
  param_50 <- 
    post_dat[["summary"]][paste0(c("beta", "lnalpha.c"), "[", stock_n, "]"), "50%", drop = FALSE] %>%
    as.data.frame() %>%
    tibble::rownames_to_column()
  
  temp <- 
    post_dat[["summary"]][ , c("2.5%", "mean", "97.5%"), drop = FALSE] %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    dplyr::rename(lb = "2.5%", mean = mean, ub = "97.5%") %>%
    dplyr::filter(grepl(paste0("^R\\[\\d+,", stock_n, "\\]|S\\[\\d+,", stock_n, "\\]"), rowname)) %>%
    dplyr::mutate(name = gsub("(^.*)\\[.*", "\\1", rowname),
                  index = as.numeric(gsub(".*\\[(\\d+),\\d\\]", "\\1", rowname)),
                  year = (name != "R") * (yr0 + index) + (name == "R") * (yr0_R + index))
  
  v_dat <-  temp %>% dplyr::filter(name == "R") %>% dplyr::select(vlb = lb, vub = ub, year)
  h_dat <-  temp %>% dplyr::filter(name == "S") %>% dplyr::select(hlb = lb, hub = ub, year)
  upper = max(quantile(c(v_dat$vub, h_dat$hub), 0.95), temp$mean)
  text_dat <-  temp %>%
    dplyr::select(mean, name, year) %>%
    tidyr::spread(name, mean) %>%
    dplyr::filter(!is.na(R) & ! is.na(S)) %>%
    dplyr::inner_join(v_dat, by = "year") %>%
    dplyr::inner_join(h_dat, by = "year")
  
  ggplot2::ggplot(text_dat, ggplot2::aes(x = S, y = R, label = year, ymin = vlb, ymax = vub, xmin = hlb, xmax = hub)) +
    ggplot2::geom_text() +
    ggplot2::geom_errorbar(linetype = 2) +
    ggplot2::geom_errorbarh(linetype = 2) +
    ggplot2::stat_function(fun=function(x){x * exp(param_50[2, 2] - param_50[1, 2] * x)}, size = 2, linetype = 2) +
    coeflines +
    ggplot2::scale_x_continuous("Spawners", limits = c(0, upper), minor_breaks = NULL, labels = scales::comma) +
    ggplot2::scale_y_continuous("Recruits", minor_breaks = NULL, labels = scales::comma) +
    ggplot2::coord_cartesian(xlim = c(0, upper), ylim = c(0, upper)) +
    ggplot2::geom_abline(slope = 1, size = 1) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::ggtitle(paste0(stock_print[which(stock_id == stock_name)], " Stock Spawner-Recruit Relationship")) +
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
  stopifnot(exists("stock_id", .GlobalEnv),
            exists("stock_print", .GlobalEnv))
  #temp <-unlist(lapply(profiles, function(x){paste0(x, c("70", "80", "90"))}))
  profile_label <- ggplot2::as_labeller(c('OYP' = "Optimum Yield Profile",
                                          'OFP' = "Overfishing Profile",
                                          'ORP' = "Optimum Recruitment Profile"))
  S.msy50 <- median(profile_dat$S.msy) 
  rug_dat <- get_BEGbounds(S.msy50)
  stock_name <- paste0(stock_print[which(stock_id == unique(profile_dat[["name"]]))], " Stock Probability Profiles")
  
  if(is.null(limit)){
    xmax <- S.msy50 * 2.25
  }
  else xmax <- limit
  
  plot <- profile_dat %>%
    dplyr::group_by(s) %>%
    dplyr::filter(s <= xmax) %>%
    tidyr::gather("key", "prob", -s, -name, -S.msy, factor_key = TRUE) %>%
    dplyr::mutate(profile = factor(gsub("([A-Z]+)[0-9]+", "\\1", key),
                                   levels = c("OYP", "OFP", "ORP")),
                  max_pct = gsub("[A-Z]+([0-9]+)", "\\1", key)) %>%
    ggplot2::ggplot(ggplot2::aes(x = s, y = prob, linetype = max_pct)) +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous("Spawners", limits = c(0, xmax), labels = scales::comma) +
    ggplot2::scale_y_continuous("Probability", breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
    ggplot2::scale_linetype_manual(name = "Percent of Max.", values = c("dotted", "dashed", "solid"))+
    ggplot2::facet_grid(profile ~ ., labeller = profile_label) +
    ggplot2::theme_bw(base_size = 13) +
    ggplot2::ggtitle(stock_name) +
    ggplot2::theme(legend.position = "bottom")
  
  if(rug == TRUE) {
    plot2 <- plot +     
      ggplot2::geom_rug(ggplot2::aes(x = lb), data = rug_dat, inherit.aes = FALSE, sides = "b", color = "darkgrey") +
      ggplot2::geom_rug(ggplot2::aes(x = ub), data = rug_dat, inherit.aes = FALSE, sides = "b", color = "black")
  }
  else plot2 <- plot
  
  if(!anyNA(goal_range)) {
    dat <- data.frame(xmin = unname(goal_range[1]), xmax = unname(goal_range[2]), ymin = -Inf, ymax = Inf)
    plot2 + ggplot2::geom_rect(ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                               data = dat,
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
#' @param post_dat The posterior object from the SRA model of class jagsUI
#' @param stock_name A character element specifying the stock to plot.
#'
#' @return A figure
#'
#' @examples
#' get_ids()
#' plot_rickeryear(post, stock_id[1])
#' plot_rickeryear(post, "East Susitna")
#' lapply(stock_id, plot_rickeryear, post_dat = post)
#'
#' @export
plot_rickeryear <- function(post_dat, stock_name){
  stopifnot(exists("year_id", .GlobalEnv),
            exists("stock_id", .GlobalEnv),
            exists("age_max", .GlobalEnv))
  yr0 <- as.numeric(min(year_id)) - 1
  yr0_R <- yr0 - age_max
  
  stock_n <- which(stock_id == stock_name)
  
  text <- 
    post_dat$summary %>%
    as.data.frame() %>% 
    dplyr::select(median = "50%") %>%
    tibble::rownames_to_column() %>%
    dplyr::filter(grepl(paste0("^R\\[\\d+,", stock_n, "\\]|S\\[\\d+,", stock_n, "\\]"), rowname)) %>%
    dplyr::mutate(name = gsub("(^.*)\\[.*", "\\1", rowname),
                  index = as.numeric(gsub(".*\\[(\\d+),\\d\\]", "\\1", rowname)),
                  year = (name != "R") * (yr0 + index) + (name == "R") * (yr0_R + index)) %>%
    dplyr::select(median, name, year) %>%
    tidyr::spread(name, median) %>%
    dplyr::filter(!is.na(R) & ! is.na(S))
  
  lines <- 
    post_dat$summary %>%
    as.data.frame() %>%
    dplyr::select(value = mean) %>%
    tibble::rownames_to_column()  %>% 
    dplyr::filter(grepl(paste0("^lnalpha.vec\\[\\d+,", stock_n, "\\]"), rowname)) %>%
    dplyr::mutate(year = round(as.numeric(year_id[as.numeric(gsub("^lnalpha.vec\\[(\\d+),\\d\\]", "\\1", rowname))]), -1), 
                  beta = as.numeric(post_dat$summary[grepl(paste0("beta\\[", stock_n, "\\]"), rownames(post_dat$summary)), "mean"])) %>%
    dplyr::select(year, value, beta) %>%
    as.matrix() %>%
    plyr::alply(1, function(coef) {ggplot2::stat_function(fun = function(x){x * exp(coef[2] - coef[3] * x)}, 
                                                          ggplot2::aes(color = as.character(coef[1])))})
  
  upper = max(text[, c("R", "S")]) * 1.1
  
  ggplot2::ggplot(text, ggplot2::aes(x = S, y = R, label = year)) +
    ggplot2::geom_text() +
    lines +
    ggplot2::scale_x_continuous("Spawners", limits = c(0, upper), minor_breaks = NULL, labels = scales::comma) +
    ggplot2::scale_y_continuous("Recruits", limits = c(0, NA), minor_breaks = NULL, labels = scales::comma) +
    ggplot2::scale_color_discrete("Decade") +
    ggplot2::coord_cartesian(xlim = c(0, upper), ylim = c(0, upper)) +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(stock_name) +
    ggplot2::theme(strip.background = ggplot2::element_rect(colour="white", fill="white"))
}


#' State Variable Plot
#'
#' Produces a plot of escapement, recruitment, total run, Ricker residuals and/or harvest rate plotted with 95% confidence envelopes
#'  for one or all of the stock groups. 
#'
#' @param post_dat The posterior object from the SRA model of class jagsUI
#' @param display A character element. One of the stock group names; "Deshka", "East Susitna", "Talkeetna", "Yentna", 
#' or state variable names: "Escapement", "Total Run", "Recruitment", "Harvest Rate", "Ricker Residuals". 
#' Choosing a stock group name will facet 5 state variables. Choosing a state variable will facet 4 stock groups.
#' @param rp NULL omits biological refence points, "msr" or "msy" include reference points for harvest rate and spawning abundance.
#'
#' @return A figure
#'
#' @examples
#' plot_state(post, stock_id[1])
#' plot_state(post, "East Susitna")
#' lapply(stock_id, plot_state, post_dat = post)
#'
#' @export
plot_state <- function(post_dat, display, rp = NULL){
  stopifnot(exists("year_id", .GlobalEnv),
            exists("age_max", .GlobalEnv),
            exists("stock_id", .GlobalEnv),
            rp %in% c(NULL, "msy", "msr"),
            display %in% c(stock_id, "Escapement", "Total Run", "Recruitment", "Harvest Rate", "Ricker Residuals"))
  yr0 <- as.numeric(min(year_id)) - 1
  yr0_R <- yr0 - age_max
  
  msy50 <- 
    post_dat$summary %>%
    as.data.frame() %>%
    dplyr::select(median = "50%") %>%
    tibble::rownames_to_column() %>%
    dplyr::filter(grepl(paste0(".msy\\[\\d\\]"), rowname)) %>%
    dplyr::mutate(name = factor(gsub("(.)\\.msy.*", "\\1", rowname),
                                levels = c("S", "U"),
                                labels = c("Escapement", "Harvest Rate")),
                  stock = stock_id[gsub(".*\\[(\\d)\\]", "\\1", rowname)])
  
  msr50 <- 
    post_dat$summary %>%
    as.data.frame() %>%
    dplyr::select(median = "50%") %>%
    tibble::rownames_to_column() %>%
    dplyr::filter(grepl(paste0("beta\\[\\d\\]|lnalpha.c\\[\\d\\]"), rowname)) %>%
    dplyr::mutate(msr = ifelse(grepl("^beta", rowname), 1 / median, 1-1/exp(median - 1)),
                  name = factor(ifelse(grepl("^beta", rowname), "S", "U"),
                                levels = c("S", "U"),
                                labels = c("Escapement", "Harvest Rate")), 
                  stock = stock_id[gsub(".*\\[(\\d)\\]", "\\1", rowname)])
  
  dat <-
    post_dat$summary %>%
    as.data.frame() %>%
    dplyr::select(median = "50%", lcb = "2.5%", ucb = "97.5%") %>%
    tibble::rownames_to_column() %>%
    dplyr::filter(grepl(paste0("^R\\[|S\\[|N\\[|log.resid.vec\\[|mu.Habove\\["), rowname)) %>%
    dplyr::mutate(name = factor(gsub("^(.*)\\[\\d+,\\d\\]", "\\1", rowname),
                                levels = c("S", "N", "R", "mu.Habove", "log.resid.vec"),
                                labels = c("Escapement", "Total Run", "Recruitment", "Harvest Rate", "Ricker Residuals")),
                  index = as.numeric(gsub("^.*\\[(\\d+),\\d\\]", "\\1", rowname)),
                  stock = stock_id[gsub(".*\\[\\d+,(\\d)\\]", "\\1", rowname)],
                  year = (name != c("Recruitment")) * (yr0 + index) + (name == "Recruitment") * (yr0_R + index)) %>%
    dplyr::filter(year >= yr0)
  
  if(display %in% stock_id){
    plot <-
      dat[dat$stock == display, ] %>%
      ggplot2::ggplot(ggplot2::aes(x = year, y = median)) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = lcb, ymax = ucb), inherit.aes = TRUE, alpha = 0.3) +
      ggplot2::facet_grid(name ~ ., scales = "free_y", switch = "y") +
      ggplot2::labs(x = NULL, y = NULL) +
      ggplot2::scale_x_continuous("Year", breaks = seq(min(year_id), max(year_id), 3), minor_breaks = NULL)  +
      ggplot2::scale_y_continuous(minor_breaks = NULL, labels = digits)  +
      ggplot2::geom_hline(ggplot2::aes(yintercept = 0), color = "black", linetype = 1) +
      ggplot2::theme_bw() +
      ggplot2::ggtitle(display) +
      ggplot2::theme(strip.background = ggplot2::element_rect(colour="white", fill="white"), strip.placement = "outside")
    
    if("msr" %in% rp) {
      plot <- 
        plot + 
        ggplot2::geom_hline(data = msr50[msr50$stock == display, ], ggplot2::aes(yintercept = msr), color = "red", linetype = 5)
    }
    if("msy" %in% rp) {
      plot <- 
        plot + 
        ggplot2::geom_hline(data = msy50[msy50$stock == display, ], ggplot2::aes(yintercept = median), color = "red", linetype = 2)
    }
  }
  
  if(display %in% c("Recruitment", "Ricker Residuals")) {x_label <- "Brood Year"} else {x_label = "Year"}
  if(display %in% c("Escapement", "Total Run", "Recruitment", "Harvest Rate", "Ricker Residuals")){
    plot <-
      dat[dat$name == display, ] %>%
      ggplot2::ggplot(ggplot2::aes(x = year, y = median)) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = lcb, ymax = ucb), inherit.aes = TRUE, alpha = 0.3) +
      ggplot2::facet_grid(stock ~ ., switch = "y") +
      ggplot2::labs(x = NULL, y = NULL) +
      ggplot2::scale_x_continuous(x_label, breaks = seq(min(year_id), max(year_id), 3), minor_breaks = NULL)  +
      ggplot2::scale_y_continuous(minor_breaks = NULL, labels = digits)  +
      ggplot2::geom_hline(ggplot2::aes(yintercept = 0), color = "black", linetype = 1) +
      ggplot2::theme_bw() +
      ggplot2::ggtitle(display) +
      ggplot2::theme(strip.background = ggplot2::element_rect(colour="white", fill="white"), strip.placement = "outside")
    
    if("msr" %in% rp) {
      plot <- 
        plot + 
        ggplot2::geom_hline(data = msr50[msr50$name == display, ], ggplot2::aes(yintercept = msr), color = "red", linetype = 5)
    }
    if("msy" %in% rp) {
      plot <- 
        plot + 
        ggplot2::geom_hline(data = msy50[msy50$name == display, ], ggplot2::aes(yintercept = median), color = "red", linetype = 2)
    }
  }
  
  plot
}


#' Ricker Posterior correlations plot
#'
#' Pairs plot for beta, ln(alpha), phi, S.msy and sigma.white
#'
#' @param dat_post posterior object
#' @param plot "bystock" a separate plot of each stock, "byparam" for a separate plot for each Ricker parameter
#'
#' @return A figure
#'
#' @examples
#' plot_statepairs(post)
#'
#' @export
plot_statepairs <- function(post_dat, plot){
  stopifnot(exists("stock_id", .GlobalEnv))
  pars = c("beta", "lnalpha", "phi", "sigma.white")  
  lb <- post_dat$summary[grepl("S.msy", rownames(post_dat$summary)), "2.5%"]
  ub <- post_dat$summary[grepl("S.msy", rownames(post_dat$summary)), "97.5%"] 
  index <- Reduce(intersect, lapply(1:length(stock_id), function(x) which(post_dat$sims.list$S.msy[, x] > lb[x] & post_dat$sims.list$S.msy[, x] < ub[x])))
  sample <- sample(index, size = 100)
  subset <- post_dat$sims.list[pars] %>% lapply(function(x){x[sample, ]})
  
  if(plot == "bystock") lapply(1:length(stock_id), function(y) pairs(lapply(subset, function(x) x[, y]), main = stock_id[y]))
  if(plot == "byparam") lapply(1:length(pars), function(x) pairs(subset[x], labels = stock_id, main = names(subset[x])))
}


#' Plots of composition by stock
#'
#' Faceted plot of stock composition for Susitna Drainage.
#'
#' @param input_dat telemetry data matrix
#' @param post_dat The posterior object from the SRA model of class jagsUI
#' @param plot_stocks The stocks to include in the plot. The default c("East Susitna", "Talkeetna", "Yentna").
#'
#' @return A figure
#'
#' @examples
#' plot_stock(telemetry, post)
#'
#' @export
plot_stock <- function(input_dat, post_dat, plot_stocks = c("East Susitna", "Talkeetna", "Yentna")){
  stopifnot(exists("year_id", .GlobalEnv),
            exists("age_max", .GlobalEnv),
            exists("stock_id", .GlobalEnv),
            exists("trib_id", .GlobalEnv),
            plot_stocks %in% c("East Susitna", "Talkeetna", "Yentna"))
  yr0 <- as.numeric(min(year_id)) - 1
  yr0_R <- yr0 - age_max
  
  id <- data.frame(stock = factor(rep(names(trib_id), sapply(trib_id, length)), levels = stock_id), 
                   tribn = unlist(sapply(trib_id, function(x) 1:length(x)), use.names = FALSE), 
                   trib0 = unlist(trib_id, use.names = FALSE),
                   stringsAsFactors = FALSE) %>%
    dplyr::mutate(trib = factor(trib0, levels = trib0)) %>%
    dplyr::select(-trib0) %>%
    dplyr::arrange(stock, tribn)
  
  obs_f <- function(stock){
    stock2 <- stock_print[stock == stock_id]
    input_dat[[stock]] %>%
      (function(x) {x/rowSums(x)}) %>%
      as.data.frame() %>%
      tibble::rownames_to_column() %>%
      tidyr::gather(trib0, p0, -rowname) %>%
      dplyr::mutate(trib = factor(trib0, 
                                  levels = id$trib[id$stock == stock]),
                    year = unname(year_id[rowname]),
                    stock = stock) %>%
      dplyr::filter(!is.na(p0)) %>%
      dplyr::arrange(year, dplyr::desc(trib)) %>%
      dplyr::group_by(year) %>%
      dplyr::mutate(p = cumsum(p0)) %>%
      dplyr::ungroup() %>%
      dplyr::select(year, stock, trib, mean = p)
  }
  
  obs <- 
    lapply(plot_stocks, obs_f) %>% do.call(rbind, .) %>%
    dplyr::mutate(stock = factor(stock, levels = stock_id[stock_id %in% plot_stocks], labels =  stock_print[stock_id %in% plot_stocks]))
  
  est <- 
    post_dat[["summary"]][grepl("p.S", rownames(post_dat$summary)), "mean"] %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    dplyr::mutate(year = as.numeric(gsub("^.*\\[(\\d+).*$", "\\1", rowname)) + yr0,
                  stock = factor(unname(stock_id[gsub("p.S(\\d).*", "\\1", rowname)]), levels = stock_id),
                  tribn = as.numeric(gsub("^.*,(\\d)]$", "\\1", rowname))) %>%
    dplyr::left_join(id, by = c("stock", "tribn")) %>%
    dplyr::select(stock, year, trib, mean = ".") %>%
    dplyr::filter(stock %in% plot_stocks)
  est$stock <- factor(est$stock, labels = stock_print[which(stock_id %in% plot_stocks)])
  
  pal <- RColorBrewer::brewer.pal(6, "Set1")
  breaks <- id[id$stock %in% plot_stocks, ] %>% 
    dplyr::mutate(color = unlist(lapply(sapply(which(stock_id %in% plot_stocks), function(x) sum(stock == stock_id[x]) - 1), function(x) c(pal[1:x], "black"))),
                  alpha = 1 + min(as.numeric(stock) - 1) / max(as.numeric(stock) - 1) - (as.numeric(stock) - 1) / max(as.numeric(stock) - 1)) %>%
    dplyr::select(-tribn) %>%
    dplyr::arrange(stock, trib, color, alpha)
  
  lev <- gsub("\\sRiver|\\sSusitna", "", id$trib[id$trib!="Deshka"])
  breaks$trib <- factor(gsub("\\sRiver|\\sSusitna", "", breaks$trib), levels = lev)
  est$trib <- factor(gsub("\\sRiver|\\sSusitna", "", est$trib), levels = lev)
  obs$trib <- factor(gsub("\\sRiver|\\sSusitna", "", obs$trib), levels = lev)
  col <-setNames(breaks$color, lev)
  alp <-setNames(breaks$alpha, lev)
  
  est %>%
    ggplot2::ggplot(ggplot2::aes(x = as.numeric(year), y = mean, fill = trib, alpha = trib)) +
    ggplot2::geom_area() +
    ggplot2::facet_grid(stock ~ ., switch = "y") +
    ggplot2::scale_x_continuous(breaks = seq(min(year_id), max(year_id), 4), minor_breaks = NULL) +
    ggplot2::scale_y_continuous(minor_breaks = NULL, labels = scales::percent) +
    ggplot2::scale_fill_manual(breaks = breaks$trib, values = col, name = "Population") +
    ggplot2::scale_alpha_manual(breaks = breaks$trib, values = alp, "Population") +
    ggplot2::geom_point(data = obs, ggplot2::aes(fill = trib), size = 3, shape = 21, color = "white") +
    ggplot2::labs(y = NULL, x = "Year") +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::ggtitle("Susitna River Chinook Salmon Stock Composition") +
    ggplot2::theme(strip.background = ggplot2::element_rect(colour="white", fill="white"), 
                   strip.placement = "outside")
}


#' Plot of escapement vrs. proposed goals faceted by stock
#'
#' Produces a faceted plot of model estimated escapement with 95% CI error bars overlain by proposed goal ranges for each stock.
#'
#' @param post_dat The posterior object from the SRA model of class jagsUI
#' @param goal_range A data frame with columns "stock", "lb" the lower bound of the goal range and "ub" the upper bound of the goal range.
#'
#' @return A figure
#'
#' @examples
#' plot_Swgoals(post, goals)
#'
#' @export
plot_Swgoals <- function(post_dat, goal_range){
  stopifnot(exists("year_id", .GlobalEnv))
  post_dat[["summary"]] %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    dplyr::filter(grepl("^S\\[\\d+,[1234]\\]", rowname)) %>%
    dplyr::select(rowname, Escapement = as.name("50%"), lb = as.name("2.5%"), ub = as.name("97.5%")) %>%
    dplyr::mutate(year = as.numeric(gsub("^S\\[(\\d+),\\d\\]", "\\1", rowname)) + min(as.numeric(year_id)) - 1,
                  stock = factor(stock_id[gsub("^.*\\[\\d+,(\\d)\\]", "\\1", rowname)], levels = stock_id)) %>%
    dplyr::left_join(regs, by = c("stock", "year")) %>%
    ggplot2::ggplot(ggplot2::aes(x = year, y = Escapement)) +
    ggplot2::geom_line() +
    ggplot2::geom_pointrange(ggplot2::aes(ymin = lb, ymax = ub, color = reg), size = 0.75, linetype = 2) +
    ggplot2::geom_rect(data = goals_df, ggplot2::aes(x = NULL, y = NULL, xmin = -Inf, xmax = Inf, ymin = lb, ymax = ub), fill = "red", alpha = 0.2) +
    ggplot2::scale_x_continuous("Year", breaks = seq(as.numeric(year_id[1]), as.numeric(year_id[length(year_id)]), 3), minor_breaks = NULL) +
    ggplot2::scale_y_continuous(minor_breaks = NULL, labels = scales::comma) +
    ggplot2::facet_grid(stock ~ ., scales = "free_y") +
    ggplot2::theme_bw() +
    ggplot2::labs(color = "Management Action")
}


#' Plots of Aerial Survey detectability
#'
#' Estimated and observed aerial survey detectability in the Susitna River drainage.
#'
#' @param post_dat The posterior object from the SRA model of class jagsUI
#'
#' @return A figure
#'
#' @examples
#' plot_theta(post)
#'
#' @export
plot_theta <- function(post_dat){
  stopifnot(exists("year_id", .GlobalEnv),
            exists("stock_id", .GlobalEnv),
            exists("trib_id", .GlobalEnv))
  
  yr0 <- as.numeric(min(year_id)) - 1
  
  id0 <- lapply(trib_id, function(x){x[!grepl("Other", x)]})
  names(id0) <- names(trib_id)
  id <- data.frame(stock = factor(rep(names(id0), sapply(id0, length)), levels = stock_id), 
                   tribn = unlist(sapply(id0, function(x) 1:length(x)), use.names = FALSE), 
                   tribn2 = 1:length(unlist(id0)),
                   trib = unlist(id0, use.names = FALSE))
  
  theta_est <- 
    post_dat[["summary"]][, c("mean", "sd")] %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>% 
    dplyr::filter(grepl("^theta\\[", rowname)) %>%
    dplyr::mutate(year = as.numeric(gsub("theta\\[\\d+,(\\d+)\\]", "\\1", rowname)) + yr0,
                  tribn2 = as.numeric(gsub("theta\\[(\\d+),\\d+\\]", "\\1", rowname))) %>%
    dplyr::left_join(id, by = "tribn2") %>%
    dplyr::select(year, stock, trib, theta = mean)
  
  theta_obs1 <- data.frame(year = as.numeric(year_id), 
                           stock = unname(stock_id[1]), 
                           trib = "Deshka", 
                           theta = post_dat$data$air.S1 / (post_dat$data$weir[, "Deshka"] - post_dat$data$Hd))
  theta_obs2 <- data.frame(year = as.numeric(year_id), 
                           stock = unname(stock_id[2]), 
                           trib = "Montana", 
                           theta = post_dat$data$air.S2[, "Montana"] / post_dat$data$weir[, "Montana"])
  theta_obs3 <- data.frame(year = as.numeric(year_id), 
                           stock = unname(stock_id[2]), 
                           trib = "Willow", 
                           theta = post_dat$data$air.S2[, "Willow"] / post_dat$data$weir[, "Willow"])
  theta_obs4 <- data.frame(year = as.numeric(year_id), 
                           stock = unname(stock_id[4]), 
                           trib = "Lake", 
                           theta = (post_dat$data$air.S4[, "Lake"] * post_dat$data$tele.p2upS4[,1] / post_dat$data$tele.p2upS4[,2]) / 
                             post_dat$data$sonar)
  
  theta_obs <- 
    rbind(theta_obs1, theta_obs2, theta_obs3, theta_obs4) %>%
    dplyr::filter(!is.na(theta))
  
  pal <- RColorBrewer::brewer.pal(6, "Paired")
  breaks <- 
    id %>% 
    dplyr::select(-tribn, -tribn2) %>%
    dplyr::mutate(color = unlist(lapply(sapply(1:length(stock_id), function(x) sum(stock == stock_id[x])), function(x) pal[1:x]))) %>%
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

#' OYP comparison
#'
#' Produces a faceted plot of OYP's for 2 SR analysis.
#'
#' @param list_old list(old_profile, old_goal, old_smsy, old_label)
#' @param list_new list(new_profile, new_goal, new_smsy, new_label)
#' @param plotmax x-axis maximum
plot_OYPcompare <- function(list_old, list_new, plotmax, rows = TRUE){
  dat_rect <-data.frame(sra = c(list_old[[4]], list_new[[4]]),
                        xmin = unlist(c(list_old[[2]][[1]], list_new[[2]][[1]])),
                        xmax = unlist(c(list_old[[2]][2], list_new[[2]][2])),
                        ymin = rep(-Inf, 2),
                        ymax = rep(Inf, 2), fix.empty.names = FALSE)
  
  rug_dat <- get_BEGbounds(list_old[[3]]) %>%
    dplyr::mutate(sra = list_old[[4]]) %>%
    dplyr::bind_rows(get_BEGbounds(list_new[[3]]) %>%
                       dplyr::mutate(sra = list_new[[4]]))
  
  temp0 <- list_old[[1]] %>%
    dplyr::select(s, dplyr::starts_with("OYP")) %>%
    tidyr::gather("key", "prob", -s, factor_key = TRUE) %>%
    dplyr::mutate(sra = list_old[[4]],
                  max_pct = stringr::str_extract(key, "[0-9]+")) %>%
    dplyr::select(s, prob, max_pct, sra)
  
  dat_new <- list_new[[1]] %>%
    dplyr::select(s, dplyr::starts_with("OYP")) %>%
    tidyr::gather("key", "prob", -s, factor_key = TRUE) %>%
    dplyr::mutate(sra = list_new[[4]],
                  max_pct = stringr::str_extract(key, "[0-9]+")) %>%
    dplyr::select(s, prob, max_pct, sra)
  
  old_circle <- c(list_old[[1]]$s[which.min(abs(list_old[[1]]$s - list_old[[2]][[1]]))], list_old[[1]]$s[which.min(abs(list_old[[1]]$s - list_old[[2]][[2]]))])
  new_circle <- c(dat_new$s[which.min(abs(dat_new$s - list_new[[2]][[1]]))], dat_new$s[which.min(abs(dat_new$s - list_new[[2]][[2]]))])
  dat_point <- list_old[[1]] %>%
    dplyr::filter(s %in% old_circle) %>%
    dplyr::mutate(sra = list_old[[4]]) %>%
    dplyr::select(s, sra, prob = OYP80) %>%
    dplyr::bind_rows(dplyr::filter(dat_new, s %in% new_circle  & max_pct == "80") %>%
                       dplyr::select(s, prob, sra))
  
  temp <- 
    dplyr::bind_rows(dat_new, temp0) %>%
    dplyr::filter(s <= plotmax) %>%
    ggplot2::ggplot(ggplot2::aes(x = s, y = prob, linetype = max_pct)) +
    ggplot2::geom_line() +
    ggplot2::geom_rect(ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                       dat_rect, inherit.aes = FALSE, fill = "red", alpha = 0.2) +
    ggplot2::geom_point(ggplot2::aes(x = s, y = prob), dat_point, inherit.aes = FALSE, shape = 1, size = 3, stroke = 2) +
    ggplot2::geom_rug(ggplot2::aes(x = lb), data = rug_dat, inherit.aes = FALSE, sides = "b", color = "darkgrey") +
    ggplot2::geom_rug(ggplot2::aes(x = ub), data = rug_dat, inherit.aes = FALSE, sides = "b", color = "black") +
    ggplot2::scale_x_continuous("Spawners", breaks = seq(0, plotmax, plotmax/5), limits = c(0, plotmax), labels = scales::comma) +
    ggplot2::scale_y_continuous("Probability", breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
    ggplot2::scale_linetype_discrete(name = "Percent of Max.", )+
    ggplot2::facet_grid(. ~ sra) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")
  if(rows == TRUE){temp + ggplot2::facet_grid(. ~ sra)} else {temp + ggplot2::facet_grid(sra ~ .)}
}
