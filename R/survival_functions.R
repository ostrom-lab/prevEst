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

#' Apply assumption of death after observation period
#' @description This function works within a survival formatting function to apply an assumption of death after observation period
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
#' @export

population_survival <- function(life.table=life.table, 
                                survival.data=survival.data,
                                prevYear = NULL,
                                years.observed.surv=NULL) {
  
  yrPrev <- agePrev <- expected <- period <- survival <- ageDiag <-  desc <- yrDiag <-  NULL
  
  `%>%` <- dplyr::`%>%`
  
  if(is.null(prevYear)) {
    prevYear= max(survival.data$yrDiag)
  }
  
  if(is.null(years.observed.surv)) {
    years.observed.surv <- max(survival.data$yrDiag) - min(survival.data$yrDiag)
  }

    ages <- min(survival.data$ageDiag):max(survival.data$ageDiag)

  
  life.table.analysis <- life.table %>% 
    dplyr::mutate_all(as.numeric) %>%
    dplyr::select(period, agePrev, expected, yrPrev) %>%
    dplyr::arrange(desc(period)) 
  
  full.survival.temp1 <- survival.data  %>%
    dplyr::mutate_all(as.numeric) %>%
    dplyr::mutate(period=yrPrev-yrDiag ,
                  agePrev=ageDiag+period)%>%
    dplyr::left_join(life.table.analysis , by = c("agePrev", "period","yrPrev"))  %>%
    dplyr::mutate(expected = ifelse(agePrev>=100, 0,expected),
                  survival = dplyr::case_when(!is.na(survival)~survival,
                                              TRUE~expected)) 
  
  full.survival.temp2 <- full.survival.temp1  %>%
    dplyr::filter(period >= (years.observed.surv)) %>%
    dplyr::arrange(period) %>%
    dplyr::group_by(ageDiag) %>%
    dplyr::mutate(survival = cumprod(survival)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(ageDiag,yrDiag) 
  
  full.survival <- full.survival.temp1 %>%
    dplyr::filter(period <= years.observed.surv) %>%
    dplyr::bind_rows(full.survival.temp2 %>% 
                       dplyr::filter(period > years.observed.surv)) %>%
    dplyr::arrange(ageDiag, period) %>%
    dplyr::select(-expected)
  
  return(full.survival)
  
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
                             assumption="nosurvival",
                             life.table=NULL
){
  options( dplyr.summarise.inform = FALSE)
  `%>%` <- dplyr::`%>%`
  
  yrDiag <- ageDiag <- predicted_incidence <- yrPrev <- yrDiag <- ageDiag <- expected <- period <- predicted_survival <- agePrev <- desc <- survival <- NULL
  
  
  if(is.null(observation.years)) {
    stop("Please specify years for observations.")
  }
  if(is.null(projection.years)) {
    stop("Please specify years for projections.")
  }
  if(all(is.na(data$survival))){
    stop("No survival data provided")
  }

  
  if(is.null(ages)){
    ages <- min(data$ageDiag):max(data$ageDiag)
  }
  
  if(is.null(prevYear)) {
    prevYear=max(c(observation.years,projection.years))
  }
  
  first.year <- min(c(data$yrDiag,min(projection.years)))
  final.year <- as.numeric(max(projection.years))
  
  if(is.null(years.observed.surv)) {
  years.observed.surv = length(observation.years)
  }
  
  for_projection <- data %>% 
    dplyr::mutate(period=yrPrev-yrDiag)%>%
    dplyr::left_join(life.table, by = c("ageDiag"="agePrev", "period","yrPrev")) %>% 
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
    dplyr::mutate(surv = dplyr::case_when(survival >= .99 ~ sample(seq(0.99,.999,.0001),size=1),
                                          survival <= 0.01 ~ sample(seq(0.0001,0.001,.0001),size=1),
                                          T ~ survival) ) %>%
    dplyr::filter(period != 0) %>%
    tidyr::nest(.by=ageDiag) %>%
    dplyr::mutate(predicted_survival=purrr::map(data, ~modelr::add_predictions(data=expand.grid(yrDiag = projection.years,
                                                                                                  period = 1: length(observation.years)),
                                                                               betareg::betareg(surv ~ jitter(as.numeric(yrDiag)) + as.numeric(period), data = .x),
                                                                               var="survival"))) %>%
    dplyr::select(-data) %>%
    tidyr::unnest(cols=predicted_survival) %>%
    dplyr::mutate(yrPrev=yrDiag+period)
    
  
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
    dplyr::mutate(survival=dplyr::case_when(yrDiag==yrPrev ~ 1,
                                            survival>1~survival/100,
                                            survival<=0~0,
                                            agePrev>=100~0,
                                            TRUE~as.numeric(survival)))  %>%
    dplyr::arrange(ageDiag, period, yrPrev) %>%
    dplyr::filter(period >= 0 & yrPrev==prevYear) %>%
    dplyr::select(ageDiag,agePrev, yrDiag, yrPrev, period , survival) 
  
  if (assumption=="population") {
    if(is.null(life.table)) {
      stop("No life table provided")
    } else {
      if(any(!c("period", "agePrev", "expected",'yrPrev') %in% names(life.table))){
        stop("Life tables must contain 'period', 'agePrev', 'yrPrev', and 'expected' columns \n")
      }
    
    final <- population_survival(life.table=life.table, 
                                    survival.data=final,
                                    prevYear = prevYear,
                                    years.observed.surv=years.observed.surv)
    }
  }
  if (assumption=="nosurvival") {
    final <- no_survival(prevYear = prevYear,
                                 years.observed.surv=years.observed.surv)
  }
  
  return(as.data.frame(final))
}