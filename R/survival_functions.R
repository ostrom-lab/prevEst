#' Apply survival stable after observation period
#' @description This function works within a survival formatting function to make survival stable after end of observation period
#' @param survival.data Survival dataframe 
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

#' Apply assumption of death after observation period
#' @description This function works within a survival formatting function to apply an assumption of death after observation period
#' @param survival.data Survival dataframe 
#' @param prevYear Year for which prevalence is being estimated
#' @param years.observed.surv Years after which to go to population survival
#' @export

no_survival <- function(survival.data=survival.data,
                        prevYear = NULL,
                        years.observed.surv=NULL) {
  `%>%` <- dplyr::`%>%`
  . <- period <- survival <-  yrPrev <- ageDiag <- yrPrev <- NULL
  
  
  if(is.null(prevYear)) {
    prevYear= max(survival.data$yrDiag)
  }
  
  
  if(is.null(years.observed.surv)) {
    years.observed.surv <-  max(survival.data$yrDiag) - min(survival.data$yrDiag)
  }
  
  full.survival <- survival.data  %>%
    dplyr::mutate(period=yrPrev-yrDiag ,
                  agePrev=ageDiag+period)%>%
    dplyr::mutate(survival = dplyr::case_when(period > years.observed.surv ~ 0,
                                              TRUE ~ survival))
  
  return(full.survival)
}

#' Apply population survival assumption for years after observation
#' @description This function works within a survival formatting function to have survival go to population expected survival after end of observed survival ye
#' @param survival.data Survival dataframe 
#' @param prevYear Year for which prevalence is being estimated
#' @param life.table Dataframe of SEER life table
#' @param years.observed.surv Years after which to go to population survival
#' @export

population_survival <- function(survival.data=survival.data,
                                prevYear = NULL,
                                years.observed.surv=NULL) {
  
  yrPrev <- agePrev <- expected <- period <- survival <- ageDiag <-  desc <- yrDiag <-  NULL
  
  `%>%` <- dplyr::`%>%`
  
  if(is.null(prevYear)) {
    prevYear= max(survival.data$yrDiag)
  }

  ages <- min(survival.data$ageDiag):max(survival.data$ageDiag)
  
  full.survival.temp1 <- survival.data  %>%
    dplyr::mutate_all(as.numeric) %>%
    dplyr::mutate(period=yrPrev-yrDiag ,
                  agePrev=ageDiag+period)%>%
    dplyr::mutate(expected = ifelse(agePrev>=100, 0,expected))
  
  
  if(is.null(years.observed.surv)) {
    years.observed.surv <- max(survival.data$yrDiag) - min(survival.data$yrDiag)
  } 

  if (max(full.survival.temp1$period, na.rm=TRUE)> years.observed.surv) {
    
    full.survival.temp1.popsurv <- full.survival.temp1 %>% 
      dplyr::filter(period>=(years.observed.surv))  %>%
      mutate(observed = dplyr::case_when(period>years.observed.surv~expected,
                                         TRUE~observed)) %>%
      dplyr::arrange(ageDiag,yrDiag, period) %>%
      dplyr::group_by(ageDiag,yrDiag) %>%
      dplyr::mutate(observed = cumprod(observed)) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(ageDiag,yrDiag) 
    
    full.survival <- full.survival.temp1 %>%
      dplyr::filter(period <= years.observed.surv) %>%
      dplyr::bind_rows(full.survival.temp1.popsurv %>% 
                         dplyr::filter(period > years.observed.surv)) %>%
      dplyr::arrange(ageDiag, period) %>%
      dplyr::select(-expected) 
    return(full.survival) 
  } else {
    return(full.survival.temp1)
  }
}


#' Project survival data 
#'
#' @description Project survival data  for unobserved future or past year(s) requires properly formatted data
#' @param data Survival dataframe for which prevalence is to be projected
#' @param ages Numeric vector of ages to be included
#' @param observation.years Numeric vector of observed years to build the model on
#' @param projection.years Numeric vector of years to predict survival for
#' @param prevYear Year for which prevalence is being estimated
#' @param assumption Named character, either "population" or "nosurvival"
#' @param method Method used for survival rate prediction, either "linear" (lm) or "betareg"
#' @param life.table Dataframe of SEER life table
#' @param names Named character vector pointing to column names
#' @return Predicted survival including \code{projection.years}
#' @examples
#' \dontrun{
#' project_survival(data=raw.survival,
#'                  ages= 0:85,
#'                  observation.years = NULL,               # A vector of years to build the model on
#'                  projection.years = NULL,
#'                  prevYear = NULL,
#'                  assumption = "population",
#'                  method="linear",
#'                  life.table = life.tables,
#'                  names = c("ageDiag"="Age_recode_with_single_ages_and_85",
#'                            "yrPrev"="yrPrev",
#'                            "yrDiag"="Year_of_diagnosis",
#'                            "Observed"="Observed"),
#'                  keepExtraCols=FALSE)
#'}
#' @seealso [prevEst::project_incidence()]
#' @export

project_survival <- function(data,                # Case listing survival data, having either 0/1 or Dead/Alive as indicators
                             ages = NULL,
                             observation.years = NULL,               # A vector of years to build the model on
                             projection.years = NULL,
                             years.observed.surv=NULL,
                             prevYear = NULL,
                             method="linear",
                             assumption="nosurvival"
){
  options( dplyr.summarise.inform = FALSE)
  `%>%` <- dplyr::`%>%`
  
  yrDiag <- ageDiag <- predicted_incidence <- yrPrev <- yrDiag <- ageDiag <- expected <- period <- predicted_survival <- agePrev <- desc <- survival <- NULL
  
  if(is.null(projection.years)) {
    stop("Please specify years for projections.")
  }
  if(all(is.na(data$observed))){
    stop("No survival data provided")
  }
  if(is.null(observation.years)) {
    observation.years <- unique(data$yrDiag)
  }
  if(is.null(years.observed.surv)) {
    years.observed.surv = length(observation.years)
  }
  if(is.null(ages)){
    ages <- min(data$ageDiag):max(data$ageDiag)
  }
  
  if(is.null(prevYear)) {
    prevYear=max(c(observation.years,projection.years))
  }
  
  first.year <- min(c(data$yrDiag,min(projection.years)))
  final.year <- as.numeric(max(projection.years))
  
  for_projection <- data %>% 
    dplyr::mutate(period=yrPrev-yrDiag)%>%
    dplyr::filter(ageDiag %in% ages) %>%
    dplyr::group_by(ageDiag, period) %>%
    dplyr::arrange(ageDiag, period) %>% 
    dplyr::ungroup() %>%
    dplyr::mutate_all(as.numeric) %>%
    dplyr::select(-expected)
  
  cat("Projecting ", length(projection.years), " years of survival for ",min(projection.years),"-",max(projection.years), "\n", sep = "")
  
  proj.surv  <- for_projection  %>%
    # tidyr::drop_na() %>%
    dplyr::filter(yrDiag %in% observation.years) %>%
    
    # remove ones and zeros and add noise to estimates
    dplyr::rowwise() %>%
    dplyr::mutate(surv = dplyr::case_when(observed >= .99 ~ sample(seq(0.99,.999,.0001),size=1),
                                          observed <= 0.01 ~ sample(seq(0.0001,0.001,.0001),size=1),
                                          T ~ observed) ) %>%
    dplyr::filter(period != 0) %>%
    tidyr::nest(.by=ageDiag)
  
  if (method=="linear") {
    proj.surv <- proj.surv %>%
      dplyr::mutate(predicted_survival=purrr::map(data, ~modelr::add_predictions(data=expand.grid(yrDiag = projection.years,
                                                                                                  period = 1: length(observation.years)),
                                                                                 lm(surv ~ jitter(as.numeric(yrDiag)) + as.numeric(period), data = .x),
                                                                                 var="observed"))) 
  } else if (method=="betareg") {
    proj.surv <- proj.surv %>%
      dplyr::mutate(predicted_survival=purrr::map(data, ~modelr::add_predictions(data=expand.grid(yrDiag = projection.years,
                                                                                                  period = 1: length(observation.years)),
                                                                                 betareg::betareg(surv ~ jitter(as.numeric(yrDiag)) + as.numeric(period), data = .x),
                                                                                 var="observed"))) 
  }
  
  proj.surv <- proj.surv %>%
    dplyr::select(-data) %>%
    tidyr::unnest(cols=predicted_survival) %>%
    dplyr::mutate(yrPrev=yrDiag+period,
                  expected=1)
  
  
  survival.merged <- dplyr::bind_rows(for_projection, proj.surv) %>%
    dplyr::mutate_all(as.numeric) %>%
    dplyr::arrange(ageDiag, yrDiag, period) %>%
    dplyr::mutate(agePrev=ageDiag+period)
  
  # Create skeleton dataframe and left join to "new" dataframe to handle missing values. Will default to survival = 1 if data mising.
  final <- tidyr::expand_grid(ageDiag  = ages,
                              yrDiag = first.year:final.year,
                              yrPrev = first.year:final.year) %>%
    dplyr::distinct(.keep_all=TRUE) %>%
    dplyr::mutate(period=yrPrev-yrDiag) %>%
    dplyr::arrange(ageDiag , yrDiag)  %>%
    dplyr::mutate_all(as.numeric) %>%
    dplyr::filter(period >= 0)  %>%
    dplyr::left_join(survival.merged, by = c( "ageDiag", "yrDiag", "yrPrev", "period")) %>%
    dplyr::mutate(observed=dplyr::case_when(yrDiag==yrPrev ~ 1,
                                            observed>1~observed/100,
                                            observed<=0~0,
                                            agePrev>=100~0,
                                            TRUE~as.numeric(observed)))  %>%
    dplyr::arrange(ageDiag, period, yrPrev) %>%
    dplyr::filter(period >= 0 & yrPrev==prevYear) 
  
  if (assumption=="population") {
    
    final_clean <- population_survival(survival.data=final,
                                 prevYear = prevYear,
                                 years.observed.surv=years.observed.surv)
  } else if (assumption=="nosurvival") {
    final_clean <- no_survival(survival.data=final,
                         prevYear = prevYear,
                         years.observed.surv=years.observed.surv)
  }
  
  final_clean <- final %>%
    dplyr::select(ageDiag,agePrev, yrDiag, yrPrev, period , observed) 
  
  return(final_clean)
}