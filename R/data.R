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