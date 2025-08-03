#' Apply assumption of death after observation period
#' @description This function works within a survival formatting function to apply an assumption of death after observation period
#' @export

no_survival <- function(survival.data=survival.data,years.observed.surv=years.observed.surv) {
  
  `%>%` <- dplyr::`%>%`
  
  period <- survival <-  NULL
  full.survival <- survival.data  %>%
    dplyr::mutate(survival = dplyr::case_when(period > years.observed.surv ~ 0, TRUE ~ survival))
  
  return(full.survival)
}