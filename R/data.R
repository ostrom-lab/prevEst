#' Simulated incidence data
#'
#' Simulated cancer registry incidence data
#' 
#' @docType data
#' @keywords datasets
#' @name incidence
#' @format A data frame with 1376 rows and 3 columns:
#' \describe{
#'   \item{yrDiag}{Year of diagnosis}
#'   \item{ageDiag}{Age at Diagnosis}
#'   \item{count}{Number of cases}
#'   ...
#' }
NULL
#'#' Simulated projected incidence data
#'
#' Simulated cancer registry incidence data

#' @docType data
#' @keywords datasets
#' @name incidence_est
#' @format ' A data frame with 1376 rows and 3 columns:
#' \describe{
#'   \item{yrDiag}{Year of diagnosis}
#'   \item{ageDiag}{Age at Diagnosis}
#'   \item{count}{Number of cases}
#'   ...
#' }
NULL
#' Simulated survival data (formatted)
#'
#' Simulated cancer registry survival data
#' 
#' @docType data
#' @keywords datasets
#' @name survival
#' @format  A data frame with 170,280 rows and 4 columns:
#' \describe{
#'   \item{ageDiag}{Age at Diagnosis}
#'   \item{period}{years of survival}
#'   \item{yrDiag}{Year of diagnosis}
#'   \item{survival}{Percent surviving}
#'   ...
#' }
NULL
#' Simulated survival data (unformated)
#'
#' Simulated cancer registry survival data
#' 
#' @docType data
#' @keywords datasets
#' @name survival_unformatted
#' @format A data frame with 170,280 rows and 4 columns:
#' \describe{
#'   \item{ageDiag}{Age at Diagnosis}
#'   \item{yrDiag}{Year of diagnosis}
#'   \item{yrPrev}{Year at which survival was observed}
#'   \item{Observed}{Percent surviving}
#'   ...
#' }
NULL
#' Simulated life table data
#'
#' Simulated cancer registry survival data
#'
#' @docType data
#' @keywords datasets
#' @name life.table
#' @format A data frame with 3,521 rows and 3 columns:
#' \describe{#' 
#'   \item{period}{years of survival}
#'   \item{agePrev}{Age at prevalence}
#'   \item{expected}{Percent expected to survive period}
#'   ...
#' }
NULL
#' Census data from 2018
#'#' 
#' @docType data
#' @keywords datasets
#' @name census.population
#' @format A data frame with 86 rows and 2 columns:
#' \describe{
#'   \item{age}{Age in 2018}
#'   \item{pop}{Total population}
#'   ...
#' }
NULL
#' 2000 US standard population in single ages from 0-90
#' 
#' @docType data
#' @keywords datasets
#' @name US_2000_standard_population
#' @format A data frame with 91 rows and 2 columns:
#' \describe{
#'   \item{age}{Age}
#'   \item{pop}{Population}
#'   ...
#' }
NULL
#' 2011 Canadian standard population in single ages from 0-90
#' 
#' @docType data
#' @keywords datasets
#' @name Canada_2011_standard_population
#' @format A data frame with 91 rows and 2 columns:
#' \describe{
#'   \item{age}{Age}
#'   \item{pop}{Population}
#'   ...
#' }
NULL
#' World (WHO 2000-2025) Std Million from 0-90
#' 
#' @docType data
#' @keywords datasets
#' @name World_standard_population
#' @format A data frame with 91 rows and 2 columns:
#' \describe{
#'   \item{age}{Age}
#'   \item{pop}{Population}
#'   ...
#' }
NULL