#' Apply survival stable after observation period
#' @description This function works within a survival formatting function to make survival stable after end of observation period
#' @export

fill_survival <- function(survival.data=survival.data)  {
  `%>%` <- dplyr::`%>%`
  ageDiag <- period <- survival <- NULL
  full.survival <- survival.data %>%
    dplyr::group_by(ageDiag) %>%
    dplyr::arrange(ageDiag, period) %>%
    tidyr::fill(survival, .direction = "downup") %>%
    dplyr::ungroup()
  
  return(full.survival)
}