#' Susitna River Chinook salmon ages
#'
#' Multinomial age data for Susitna River Chinook salmon
#'
#' @docType data
#'
#' @format data frame
#'   \describe{
#'   \item{year}{year sampled}
#'   \item{location}{location and type of sample}
#'   \item{x3-x78}{Number of fish sampled by total age}
#'   \item{n}{sample size}
#'   }
#'
#' @source various
"age"

#' Susitna River area codes
#'
#' SRA and MR codes with descriptors
#'
#' @docType data
#' 
#' @format tibble
"codes"

#' Mark-recapture abundance estimates
#'
#' Susitna and Yentna abundance estimates
#'
#' @docType data
#' 
#' @format dataframe
"mr"

#' Northern Cook Inlet Marine Harvest 
#'
#' Commercial and Substance harvest of Chinook salmon in North Cook inlet. Susitna numbers apportioned by GSI.
#'
#' @docType data
#'  
#' @format tibble
"Hm"

#' Susitna River Chinook Harvest 
#'
#' Susitna River Chinook salmon harvest by stock 
#'
#' @docType data
#'  
#' @format tibble
#' 
#' @source SWHS
"Ha"

#' Complete Aerial Survey data
#'
#' Aerial survey data by group with only complete surveys included.
#'
#' @docType data
#' 
#' @format tibble
"as_complete"

#' Expanded Areial Survey data
#'
#' Aerial survey data by group with missing surveys expanded by their average contribution using multiple logistic regression
#'
#' @docType data
#' 
#' @format tibble
"as_expand"

#' Weir data
#'
#' Annual weir passage estimates within the Susitna River Drainage
#'
#' @docType data
#' 
#' @format tibble
"weir"

#' Telemetry data
#'
#' Radio Telemetry final destinations
#'
#' @docType data
#' 
#' @format matrix
"telemetry"

#' Less than 500mm MEF 
#'
#' Number of Chinook salmon lt 500mm MEF and total number sampled 
#'
#' @docType data
#' 
#' @format tibble
"lt500"

#' Dataframe with Statewide Chinook Salmon Smsy and goal ranges
#'
#' Statewide Chinook Salmon Smsy and goal ranges
#'
#' @docType data
#'
#' @usage data(dat_ChinBEGs)
#'
#' @format A data frame
#'   \describe{
#'   \item{REGION}{Sport Fish Region}
#'   \item{STOCK}{Name of the Stock}
#'   \item{SMSY}{Smsy of the Stock}
#'   \item{LB}{Lower bound of the Goal}
#'   \item{UB}{Upper bound of the Goal}
#'   }
#'
#' @source Initially compiled by Steve F.
"dat_chinBEGs"
