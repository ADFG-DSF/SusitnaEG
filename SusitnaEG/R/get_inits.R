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
  list(
    Bfork.scale = runif(1, 0.1, 0.75),
    Btheta.scale = runif(11, 0.1, 0.75),
    D.scale = runif(1, 0.1, 0.5),
    Dtrib.scale = runif(1, 0.1, 0.5),
    beta = rlnorm(1, log(2e-5), 0.4),
    lnalpha = rlnorm(1, log(1.6), 0.4),
    log.resid.0 = rnorm(1, 0, 1),
    mean.log.R = rnorm(1, 11.3, 0.5),
    phi = runif(1, 0.25, 0.75),
    tau.R = runif(1, 1, 25),
    tau.white = runif(1, 1, 25),
    tau.asmain = runif(1, 1, 25),
    tau.asyent = runif(1, 1, 25),
    tau.weir = runif(1, 1, 25),
    ML1 = c(runif(2, -1, 1), NA),
    ML2 = c(runif(2, -0.1, 0.1), NA)
  )
}
