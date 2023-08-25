#' Estimate complete prevalence
#'
#' @description The function \code{prevEst} is used to estimate complete prevalence using survival and incomplete incidence data
#'
#' @param incidence Incomplete incidence dataframe containing age at diagnosis (named "ageDiag"), year of diagnosis (named "yrDiag"), and count of new cases (named "count)
#' @param survival Survival dataframe for each age at prevalence year containing:  age at diagnosis (named "ageDiag"), year of diagnosis (named "yrDiag"), observed survival rate (named "survival")
#' @param year Numeric. Year for which prevalence is to be estimated
#' @param years Numeric. Years to use for calculating prevalence. Defaults to all years included in incidence dataframe.
#' @param adjust Logical. If TRUE, rates will be age-adjusted
#' @param grouped_ages Logical. If TRUE, multi-year age groups are used for adjustment.
#' @param groups If grouped ages are used, vector of grouped age levels contain the lowest group in each age group is selected
#' @param sex_specific character string containing sex ("Male" or "Female" if requesting sex-specific rates)
#'
#' @return A dataframe with estimated prevalence by age at prevalence. If incidence data is grouped (as in the sample data),
#' then it will assume that all cases in the group (e.g. ages 0-4) have the same rates of survival. 
#'
#' @examples
#' 
#' data(incidence)
#' data(incidence_est)
#' data(survival)
#' 
#' \dontrun{
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
#'

prevEst <- function(
  incidence,                                               # Incomplete incidence dataframe containing the variables: 1) age at diagnosis, 2) year diagnosed, 3) count
  survival,                                                # Survival dataframe for each age at prevalence year containing: 1) age at diagnosis, 2) year diagnosed, 3) observed survival proportion
  year,                                                    # Complete prevalence year,
  years = NULL,
  adjust = F,                                              # Age-adjust results? If no, ignore subsequent arguments
  grouped_ages = F,                                        # If ages are grouped (i.e. 5-year age bands), then this should be TRUE
  groups = NULL,                                           # Vector of grouped age levels containing the lowest age in each group,
                                                           # (e.g. if using 5-year age bands starting at age = 0, the vector would look like c(0, 5, 10, 15, etc.))
  sex_specific = "Both sexes"                              # Use sex-specific statistics for rates? If so, choose "Male" or "Female"
  )

{
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
  

    inc <- incidence %>% dplyr::mutate_all(as.numeric)  %>%
      dplyr::distinct(.keep_all=TRUE) %>%
      dplyr::mutate(yrPrev = year,
           agePrev = (year-yrDiag) + ageDiag)
      
    surv <- survival %>% dplyr::mutate_all(as.numeric) %>%
      dplyr::distinct(.keep_all=TRUE) %>%
      dplyr::mutate(yrPrev = year,
             agePrev = (year-yrDiag) + ageDiag,
             survival = ifelse(agePrev >=100, 0, survival))
    
    
    prevest <- inc %>%
      dplyr::filter(yrDiag %in% years) %>%
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
    
      if (adjust == T) {
      
        if(sex_specific != "Both sexes") {
          census.population <- census.population %>%
            dplyr::mutate(sex = tolower(sex)) %>%
            dplyr::filter(sex == tolower(sex_specific))
        }
      
        if(grouped_ages == T){  
          
      census.population <- census.population %>%
        dplyr::mutate(age = as.character(cut(as.numeric(age), c(groups, max(groups)*2), include.lowest = F, right = F, labels = groups))) %>%
        dplyr::group_by(age) %>%
        dplyr::summarise(census_population = sum(as.numeric(census_population))) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(as.numeric(age))
      
      
      standard.population <- standard.population %>%
        dplyr::mutate(age = as.character(cut(as.numeric(age), c(groups, max(groups)*2), include.lowest = F, right = F, labels = groups))) %>%
        dplyr::group_by(age) %>%
        dplyr::summarise(standard_population = sum(as.numeric(standard_population))) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(as.numeric(age))
      
        }

    prevest <- prevest %>%
      dplyr::mutate(prevalence = ifelse(agePrev >= 85, sum(.$prevalence[which(.$agePrev >= 85)]), prevalence)) %>%
      dplyr::filter(agePrev <= 85) %>%
      dplyr::left_join(census.population, by = c("agePrev" = "age")) %>%
      dplyr::left_join(standard.population, by = c("agePrev" = "age")) %>%
      dplyr::group_by(agePrev) %>%
      dplyr::mutate(crude_rate = prevalence/census_population*100000,
             weights = standard_population/sum(standard.population$standard_population),
             adjusted_rate = crude_rate*weights,
             adjusted_lci = asht::wspoissonTest(prevalence, w = as.numeric(weights)/as.numeric(census_population), wmtype = "tcz", mult=100000)$conf.int[[1]],
             adjusted_uci = asht::wspoissonTest(prevalence, w = as.numeric(weights)/as.numeric(census_population), wmtype = "tcz", mult=100000)$conf.int[[2]]) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(as.numeric(agePrev))
    }
  
  return(prevest %>% dplyr::mutate_all(as.numeric) %>% filter(agePrev >= 0) %>% arrange(agePrev))  
}
