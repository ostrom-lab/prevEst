#' Estimate complete survival
#'
#' @param surv Incomplete survival dataframe containing age at diagnosis (named "ageDiag"), year of diagnosis (named "yrDiag"), and survival of new cases (named "observed)
#' @param names names of variables in survival dataframe 
#' @param years  Years to do regression on i.e. c(2000:2010)
#' @param life.table Dataframe of life tables with relative survival with respect to age and period
#' @return A survival dataframe with missing values imputed
#' @examples
#' 
#' \dontrun{
#' data(survival)
#' data(life.table)
#' regSurv(surv = survival,
#'    years=2019:2022,
#'    names = c("ageDiag" = "ageDiag", 
#'           "yrDiag" = "yrDiag",
#'           "period" = "period",
#'           "observed" = "survival"),
#'          life.table = life.table)
#' }
#' @export

regSurv <- function(
    surv,
    years=NULL,
    names = c("ageDiag" = "ageDiag", 
              "yrDiag" = "yrDiag",
              "period" = "period",
              "observed" = "survival"),
    life.table
  ){
  
  . <- ageDiag <- mutate <- period <- var <- expected <- yrDiag <- data <- observed <- surv_pred <- survival <-  NULL
  
  `%>%` <- dplyr::`%>%`
  
  surv <- 
    data.frame(ageDiag = surv[[names[["ageDiag"]]]],
               yrDiag = as.numeric(surv[[names[["yrDiag"]]]]),
               period = as.numeric(surv[[names[["period"]]]]),
               observed = as.numeric(surv[[names[["observed"]]]])) %>%
      dplyr::left_join(life.table %>% dplyr::mutate(ageDiag=(agePrev-period)), by = c("ageDiag", "period")) %>%
      dplyr::group_by(ageDiag) %>%
      dplyr::arrange(period) %>%
      dplyr::mutate(expected = dplyr::case_when(ageDiag+period >= 100 ~ 0.001,
                                  T ~ cumprod(expected)),
             observed = dplyr::case_when(ageDiag + period >= 100 ~ 0.001,
                                  T ~ observed))

    message("Regressing missing survival data \n")
    
    # Guess years for regression to make things easy
    # Since the models are made nested by ageDiag,
    # If any age at diagnosis is missing data for all periods (i.e. all(is.na(x$expected)) == T),
    # It supplements with the lifetables provided

    expector <- function(x){
    if(sum(grepl("expected", names(x))) == 1 & is.na(var(x$observed, na.rm = T))){
      x = x %>% dplyr::mutate(surv = dplyr::case_when(expected >= 1 ~ 0.999,
                                    expected <= 0 ~ 0.001,
                                    T ~ expected),
                       surv_pred = surv)
      } else {
      x = x %>% dplyr::mutate(surv = dplyr::case_when(observed >= 1 ~ 0.999,
                                    observed <= 0 ~ 0.001,
                                    T ~ observed))
      
      try = try(suppressWarnings(modelr::add_predictions(x, betareg::betareg(surv ~ period + yrDiag, data = x), var = "surv_pred")), silent = T)
          
          if (inherits(try, "try-error")){ 
            x = x %>% dplyr::mutate(surv_pred = expected) 
          } else {
            x = try
            }
      }
      return(x)
    }
    regsurv <- surv %>%
      dplyr::arrange(ageDiag, yrDiag) %>%
      dplyr::group_by(ageDiag) %>%
      tidyr::nest() %>%
      dplyr::mutate(predicted_surv = purrr::map(data, ~expector(.x))) %>%   
      dplyr::select(-data) %>%
      tidyr::unnest(tidyselect::everything()) %>%
      dplyr::mutate(survival = dplyr::case_when( is.na(observed) ~  surv_pred,
                                          TRUE~ observed) %>% round(., 3)) %>%
      dplyr::ungroup() %>%
      dplyr::select(ageDiag, yrDiag, survival, period) 
    
  return(regsurv)
}
