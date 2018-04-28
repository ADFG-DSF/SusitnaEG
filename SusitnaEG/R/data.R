#' Susitna River Chinook salmon ages
#'
#' Dataframe multinomial age data for Susitna River Chinook salmon
#'
#' @docType data
#'
#' @format data frame
#'   \describe{
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
#' Susitna River Chinook salmon harvest by stock group (see codes.rda) 
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

#' Expanded Areial SUrvey data
#'
#' Aerial survey data by group with missing surveys expanded by their average contribution using multiple logistic regression
#'
#' @docType data
#' 
#' @format tibble
"as_expand"

