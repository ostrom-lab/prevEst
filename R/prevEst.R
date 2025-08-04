#' Estimate complete prevalence
#'
#' @description The function \code{prevEst} is used to estimate complete prevalence using survival and incomplete incidence data
#'
#' @param incidence Incomplete incidence dataframe containing age at diagnosis (named "ageDiag"), year of diagnosis (named "yrDiag"), and count of new cases (named "count)
#' @param survival Survival dataframe for each age at prevalence year containing:  age at diagnosis (named "ageDiag"), year of diagnosis (named "yrDiag"), observed survival rate (named "survival")
#' @param year Numeric. Year for which prevalence is to be estimated
#' @param years Numeric. Years to use for calculating prevalence. Defaults to all years included in incidence dataframe.
#' @param grouped_ages Logical. If TRUE, multi-year age groups are used for adjustment.
#' @param groups If grouped ages are used, vector of grouped age levels contain the lowest group in each age group is selected

#' @return A dataframe with estimated prevalence by age at prevalence. If incidence data is grouped (as in the sample data),
#' then it will assume that all cases in the group (e.g. ages 0-4) have the same rates of survival. 
#'
#' @examples
#' 
#' \dontrun{
#' data(incidence)
#' data(incidence_est)
#' data(survival)
#' 
#' prevEst(incidence = regPrev(incidence = incidence,
#'                              incidence_est = incidence_est,
#'                             regYr = c(2001:2017)),
#'         survival = format_survival(survival,
#'                                    ages = c(0:85),
#'                                    years = c(1995:2018),
#'                                    assumption = "population",
#'                                    names = c("ageDiag" = "ageDiag",
#'                                              "yrDiag" = "yrDiag",
#'                                              "Observed" = "survival"),
#'                                    life.table = life.table),
#'         year = 2018,
#'         years = 2010:2018)         
#' }
#' @export
#' 

prevEst <- function(
  incidence,                                               # Incomplete incidence dataframe containing the variables: 1) age at diagnosis, 2) year diagnosed, 3) count
  survival,                                                # Survival dataframe for each age at prevalence year containing: 1) age at diagnosis, 2) year diagnosed, 3) observed survival proportion
  year,                                                    # Complete prevalence year,
  years = NULL,
  grouped_ages = F,                                        # If ages are grouped (i.e. 5-year age bands), then this should be TRUE
  groups = NULL                                           # Vector of grouped age levels containing the lowest age in each group,
  # (e.g. if using 5-year age bands starting at age = 0, the vector would look like c(0, 5, 10, 15, etc.))
  )

{
  
  yrDiag <- ageDiag <- agePrev <- count <- final <- prevalence <- sex <- age <- standard_population <- crude_rate <- weights <- NULL
  
  options(dplyr.summarise.inform = FALSE)
  `%>%` <- dplyr::`%>%`
  
  
  if (is.null(incidence)){
    stop("Incidence dataframe is NULL")
  } else if (is.null(survival)){
    stop("Survival dataframe is NULL")
  } else if (is.null(year)){
    stop("Prevalence year not specified")
  } else if (is.null(years)){
    years <- unique(incidence$yrDiag)
  } 
  

    inc <- incidence %>%
      dplyr::mutate_all(as.numeric)  %>%
      dplyr::filter(yrDiag %in% years) %>%
      dplyr::distinct(.keep_all=TRUE) %>%
      dplyr::mutate(yrPrev = year,
           agePrev = (year-yrDiag) + ageDiag) %>%
      tidyr::drop_na() 
      
    surv <- survival %>% 
      dplyr::mutate_all(as.numeric) %>%
      dplyr::distinct(.keep_all=TRUE) %>%
      dplyr::mutate(yrPrev = year,
             agePrev = (year-yrDiag) + ageDiag,
             survival = ifelse(agePrev >=100, 0, survival)) %>%
      dplyr::filter(yrDiag %in% years) %>%
      dplyr::filter(agePrev <= max(inc$agePrev)) %>%
      tidyr::drop_na() 
    
    prevest <- inc %>%
      dplyr::left_join(surv, by = c("ageDiag", "yrDiag", "agePrev","yrPrev")) %>%
      dplyr::mutate(final = count*survival) %>%
      dplyr::group_by(agePrev) %>%
      dplyr::summarise(prevalence = round(sum(final, na.rm=TRUE))) %>%
      dplyr::ungroup() %>%
      dplyr::arrange()
    
    if(grouped_ages == T){
      prevest<- prevest %>%
        dplyr::mutate(agePrev = as.character(cut(as.numeric(agePrev), c(groups, max(groups)*2), include.lowest = F, right = F, labels = groups))) %>%
        dplyr::group_by(agePrev) %>%
        dplyr::summarise(prevalence  = sum(prevalence)) %>%
        dplyr::ungroup()
    }
    
    prevest <- prevest %>% 
      dplyr::mutate_all(as.numeric) %>% 
      dplyr::filter(agePrev >= 0) %>% 
      dplyr::arrange(agePrev)
    
  return(prevest)  
}
