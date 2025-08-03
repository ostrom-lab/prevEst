#' Format survival data
#' @description This function formats survival data for streamlined use with functions included in this package. While designed to work with case-listing, it can also work with already summarized survival data provided by the SEER*Stat "Survival Session".
#'
#' @param data Survival dataframe
#' @param ages Numeric vector
#' @param years Numeric vector
#' @param SurvType Specify observed or expected survival
#' @param prevYear Numeric vector
#' @param assumption Named character vector
#' @param life.table Dataframe of SEER life table
#' @param years.observed.surv numeric
#' @param names Named vector
#' @param keepExtraCols Logical
#' @details The prevEst function requires properly formatted incidence and survival data. This function, the counterpart to [format_incidence()],is designed 
#' to take SEER-like incidence data and format it for the [prevEst()] function. Three columns are necessary: 1) age at diagnosis, 2) year of diagnosis, 
#' and 3) the observed survival for the combination of the two. While these functions are not mandatory, they help prepare the data for [prevEst()] with simple transformation steps.
#' @return A formatted survival dataframe.
#'
#' @examples
#' \dontrun{
#' data(survival)
#' data(life.table)
#' 
#' format_survival(data=survival,
#'                 ages = c(0:85),
#'                 years = c(2015:2018),
#'                 SurvType= c("Observed"),
#'                 assumption = "nosurvival",
#'                 years.observed.surv = NULL,
#'                 names = c("ageDiag" = "ageDiag",
#'                           "yrDiag" = "yrDiag",
#'                           "Observed" = "survival",
#'                           "Expected" = "Expected"),
#'                 keepExtraCols = FALSE)
#'                 
#' format_survival(data=survival,
#'                 ages = c(0:85),
#'                 years = c(1995:2018),
#'                 SurvType= c("Observed"),
#'                 assumption = "population",
#'                 names = c("ageDiag" = "ageDiag",
#'                           "yrDiag" = "yrDiag",
#'                           "yrPrev"="yrPrev",
#'                           "period"="period",
#'                           "Observed" = "survival",
#'                           "expected" = "Expected"),
#'                 life.table = life.table,
#'                 keepExtraCols = FALSE)
#' 
#'
#'
#' }
#' @seealso [prevEst::format_incidence()] The analogous function that formats incidence data
#' @export

format_survival <- function(data, # Survival data to be formatted
                            ages = NULL, # Ages included in the data to be included in the output
                            years = NULL, # Years included in the data to be included in the output
                            prevYear = NULL, # Year to calculate prevalence for. Defaults to the highest years in data.
                            assumption="nosurvival", 
                            years.observed.surv = NULL,
                            SurvType = "Observed",
                            life.table=NULL,
                            names=c("ageDiag"="ageDiag",
                                    "yrDiag"="yrDiag",
                                    "yrPrev"="yrPrev",
                                    "period"="period",
                                    "Observed"="Observed",
                                    "expected" ="Expected"),
                            keepExtraCols=FALSE
) {
  
  require(dplyr)
  require(tidyr)
  
  yrDiag <- yrPrev <- ageDiag <- period <- agePrev <- expected <- desc <- survival <- NULL
  
  options(dplyr.summarise.inform = FALSE)
  `%>%` <- dplyr::`%>%`
  
  if(is.null(years)) {
    years = sort(unique(data[[names[["yrDiag"]]]]))
  }
  if(is.null(prevYear)) {
    prevYear = max(years)
  } 
  
  if( !("yrPrev" %in% names (names))) {
    data$yrPrev <- data$yrDiag + data$period
    names <- c(names,"yrPrev"="yrPrev")
  }
  
  
  new <- data.frame(ageDiag  = as.numeric(gsub("\\D", "", data[[names[["ageDiag"]]]])),
                    yrDiag = as.numeric(gsub("\\D", "", data[[names[["yrDiag"]]]])),
                    yrPrev = as.numeric(gsub("\\D", "", data[[names[["yrPrev"]]]])),
                    survival = as.numeric(data[[names[[SurvType]]]])) %>%
    dplyr::distinct(.keep_all=TRUE) %>%
    dplyr::filter(yrDiag %in% years & yrPrev == prevYear) %>%
    dplyr::mutate(period = yrPrev-yrDiag,
           agePrev = ageDiag+period,
           survival = dplyr::case_when(any(survival>1) ~ survival/100,
                                TRUE ~ survival)) %>%
    dplyr::arrange(yrDiag, ageDiag)
  
  if(keepExtraCols==TRUE) {
    new <- new %>%
      dplyr::bind_cols(data %>% dplyr::select(-names))
  }
  
  if (is.null(years.observed.surv )) {
    years.observed.surv = length(unique(new$yrDiag))
  }
  
  # Create skeleton dataframe and left join to "new" dataframe to handle missing values.
  skeleton <- tidyr::expand_grid(ageDiag  = ages,
                                 yrDiag = years,
                                 yrPrev = prevYear)  %>%
    dplyr::distinct(.keep_all=TRUE) %>%
    dplyr::mutate(period=yrPrev-yrDiag,
                  agePrev=ageDiag+period) %>%
    dplyr::arrange(ageDiag , yrDiag)
  
  
  temp.survival <- skeleton %>%
    dplyr::left_join(new, by = names(skeleton))
  
  if (assumption == "population") {
    if (is.null(life.table)) {
      stop("Life table must be provided for population survival \n")
    } else {
      message("Applying population-level survival \n")
    }
    if(any(!c("period", "agePrev", "expected") %in% names(life.table))){
      stop("Life tables must contain 'period', 'agePrev', and 'expected' columns \n")
    }
    
    if( !("yrPrev" %in% names (life.table))) {
      life.table$yrPrev <- prevYear
    }
    
    full.survival <- population_survival(life.table=life.table, survival.data=temp.survival,years.observed.surv=years.observed.surv,ages=ages) 

  } else if (assumption=="nosurvival") {
    message("Applying no survival assumption \n")
    full.survival <- no_survival(survival.data=temp.survival,years.observed.surv=years.observed.surv)
    
  } else if (assumption=="fill") {
    
    message("Filling survival \n")
    
    full.survival<- fill_survival(temp.survival)
  } 
  
  final <- full.survival %>%
    dplyr::filter(yrPrev==prevYear) %>%
    dplyr::arrange(ageDiag,period) %>%
    dplyr::mutate(survival=dplyr::case_when(survival > 1 ~ 1,
                                     survival < 0 ~ 0,
                                     agePrev >= 100 ~ 0,
                                     period == 0 ~ 1,
                                     TRUE ~ round(as.numeric(survival), 3))) %>%
    dplyr::mutate_all(as.numeric)
  
  return(as.data.frame(final))
}
