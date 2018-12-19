#' Age data
#'
#' Multinomial age data for Susitna River Chinook salmon
#'
#' @docType data
#'
#' @format data frame
#'   \describe{
#'   \item{year}{year sampled}
#'   \item{location}{location and type of sample}
#'   \item{stock}{Stock group of sample}
#'   \item{x3-x78}{Number of fish sampled by total age}
#'   \item{n}{sample size}
#'   }
#'
#' @source various
"age"

#' Mark-recapture abundance estimates
#'
#' Mark-recapture abundance estimates by stock
#'
#' @docType data
#' 
#' @format list
#'   \describe{
#'   \item{mr}{Abundance estimates}
#'   \item{cv_mr}{Coefficient of variation for abundance estimates}
#'   }
"mr"

#' Marine Harvest data
#'
#' Commercial and Substance harvest of Chinook salmon in North Cook inlet. Apportioned by GSI.
#'
#' @docType data
#'  
#' @format tibble
"Hm"

#' Inriver Harvest data
#'
#' Susitna River Chinook salmon harvest by stock 
#'
#' @docType data
#'  
#' @format tibble
#' 
#' @source SWHS
"Ha"

#' Deshka Harvest data
#'
#' Deshka River Chinook salmon harvest upstream of the weir 
#'
#' @docType data
#'  
#' @format tibble
#' 
#' @source SWHS
"Hm"

#' Aerial Survey data
#'
#' Aerial survey data by stock and tributary.
#'
#' @docType data
#' 
#' @format list
#'  \describe{
#'   \item{Deshka}{A matrix of aerial survey data from the Deshka River}
#'   \item{East Susitna}{A matrix of aerial survey data from the East Susitna stock group}
#'   \item{Talkeetna}{A matrix of aerial survey data from the Talkeetna drainage}
#'   \item{Yentna}{A matrix of aerial survey data from the Yentna drainage}
#'   }
"as"

#' Weir data
#'
#' Annual weir passage estimates within the Susitna River Drainage
#'
#' @docType data
#' 
#' @format matrix
"weir"

#' Telemetry data
#'
#' Radio Telemetry final destinations by Stock
#'
#' @docType data
#' 
#' @format list
#'  \describe{
#'   \item{East Susitna}{A matrix of radio telemetry final destinations in the East Susitna area}
#'   \item{N_East Susitna}{Row sums of the East Susitna matrix}
#'   \item{Talkeetna}{A matrix of radio telemetry final destinations in the Talkeetna drainage}
#'   \item{N_Talkeetna}{Row sums of the Talkeetna matrix}
#'   \item{Yentna}{A matrix of radio telemetry final destinations in the Yentna drainage}
#'   \item{N_Yentna}{Row sums of the Yentna matrix}
#'   }
"telemetry"

#' Chinook salmon size data 
#'
#' Number of Chinook salmon by size class (500mm MEFT boundary) 
#'
#' @docType data
#' 
#' @format data.frame
#'   \describe{
#'   \item{year}{Year collected}
#'   \item{age}{Age class}
#'   \item{n_small}{Number of fish sampled less than 500mm MEFT}
#'   \item{n}{Total Number of fish sampled}
#'   \item{p)small}{Percentage of fish less than 500mm MEFT}
#'   }
"lt500"

#' Statewide Chinook Salmon Smsy and goal ranges
#'
#' Statewide Chinook Salmon Smsy and goal ranges
#'
#' @docType data
#'
#' @usage data(dat_chinBEGs)
#'
#' @format data.frame
#'   \describe{
#'   \item{Region}{Sport Fish Region}
#'   \item{Stock}{Name of the Stock}
#'   \item{Smsy}{Smsy of the Stock}
#'   \item{lb}{Lower bound of the Goal}
#'   \item{ub}{Upper bound of the Goal}
#'   }
#'
#' @source Initially compiled by Steve F.
"chinBEGs"
