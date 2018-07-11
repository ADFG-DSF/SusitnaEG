#' Initial values for Jags model
#'
#' Function to generate random but constrainded initial values.
#'
#'
#' @return A list of initial values
#'
#' @examples
#' get_inits()
#'
#' @export
get_inits <- function(){
  stopifnot(exists("age_id", .GlobalEnv))
  
  list(
    Dscale.S2 = runif(1, 0.1, 0.5),
    Dscale.S3 = runif(1, 0.1, 0.5),
    Dscale.S4 = runif(1, 0.1, 0.5),
    Dscale.S5 = runif(1, 0.1, 0.5),
    beta = rlnorm(5, log(2e-5), 0.4),
    lnalpha = rlnorm(5, log(1.6), 0.4),
    log.resid.0 = rnorm(5, 0, 1),
    mean.log.R = rnorm(5, 11.3, 0.5),
    phi = runif(5, 0.25, 0.75),
    tau.R = runif(1, 1, 25),
    tau.white = runif(5, 1, 25),
    z.air = runif(16, 0, 0.5),
    g.air = runif(16, 0, 0.5),
    tau.weir = runif(1, 1, 25),
    ML1 = c(runif(length(age_id) - 1, -1, 1), NA),
    ML2 = c(runif(length(age_id) - 1, -0.1, 0.1), NA)
  )
}
