#' Format survival data
#' @description This function formats survival data for streamlined use with functions included in this package. While designed to work with case-listing, it can also work with already summarized survival data provided by the SEER*Stat "Survival Session".
#'
#' @param data Survival dataframe
#' @param SurvType Specify observed or expected survival
#' @param prevYear  Year of prevalence
#' @param ages Numeric vector. If not included, defaults to all ages in data.
#' @param years Numeric vector. If not specified, defaults to all years in data.
#' @param names Named vector
#' @details The prevEst function requires properly formatted incidence and survival data. This function, the counterpart to [format_incidence()],is designed 
#' to take SEER-like incidence data and format it for the [prevEst()] function. Three columns are necessary: 1) age at diagnosis, 2) year of diagnosis, 
#' and 3) the observed survival for the combination of the two. While these functions are not mandatory, they help prepare the data for [prevEst()] with simple transformation steps.
#' @return A formatted survival dataframe.
#'
#' @seealso [prevEst::format_incidence()] The analogous function that formats incidence data
#' @export


format_survival <- function(data, # Survival data to be formatted
                            ages=NULL,                # A vector of ages to be included in the output
                            covered_years=NULL,               # A vector of years to be included in the output
                            prevYear=NULL,
                            names=c("ageDiag"="ageDiag",
                                    "yrDiag"="yrDiag",
                                    "yrPrev"="yrPrev",
                                    "period"="period",
                                    "Observed"="Observed",
                                    "Expected" ="Expected")
) {
  `%>%` <- dplyr::`%>%`
  
  . <- yrDiag <- ageDiag <- yrPrev <- NULL
  
  if( !("yrPrev" %in% names (names))) {
    data$yrPrev <- data$yrDiag + data$period
    names <- c(names,"yrPrev"="yrPrev")
  }
  
  new <- data %>%
    dplyr::rename( c("ageDiag"=names[["ageDiag"]],
                     "yrDiag"=names[["yrDiag"]], 
                     "yrPrev" =names[["yrPrev"]],
                     "observed"=names[["Observed"]],
                     "expected"=names[["Expected"]])) %>%
    dplyr::select(ageDiag,yrDiag,yrPrev,observed,expected) 
  
  
  if( max(new$observed, na.rm=TRUE)>1 ) {
    new <- new %>%
      dplyr::mutate_at(.vars=vars(observed,expected),~as.numeric(.)/100)
  } else {
    new <- new %>%
      dplyr::mutate_at(.vars=vars(observed,expected),as.numeric)
  }
  
  new <- new %>%
    dplyr::mutate(observed = dplyr::case_when(yrDiag==yrPrev ~ 1,
                                              max(.$observed, na.rm=TRUE) >1 ~ observed/100,
                                              TRUE ~ observed))  %>%
    dplyr::distinct(.keep_all=TRUE)  
  
  if (is.null(ages)) {
    ages <- min(new$ageDiag):max(new$ageDiag)
  }
  if ( is.null(covered_years)) {
    covered_years <- min(new$yrDiag):max(new$yrDiag)
  }
  
  if(is.null(prevYear)) {
    prevYear = covered_years
  } 
  
  skeleton <- tidyr::expand_grid(ageDiag = ages,
                                 yrDiag =  as.numeric(covered_years)) %>%
    dplyr::arrange(ageDiag, yrDiag)
  
  final <-  dplyr::left_join(skeleton, new, by = c("ageDiag", "yrDiag")) %>%
    dplyr::arrange(ageDiag, yrDiag)
  
  return(final)
}


#' Format life table data
#'
#' @description This function formats life table data for ease of use with functions included in this package. 
#' @param data Incidence dataframe
#' @param ages Numeric vector. If not included, defaults to all ages in data.
#' @param years Numeric vector. If not specified, defaults to all years in data.
#' @param names Named character vector
#' @details Survival projection functions in prevEst  requires properly formatted life table data. This function, the counterpart to [format_incidence()] and [format_survival()], is designed 
#' to take SEER-like life table data and format it for the [prevEst()] function. Four columns are necessary: 1) age, 2) year, 3) survival interval (in years), 
#' and 4) expected interval survival. While these functions are not mandatory, they help prepare the data for [prevEst()] with simple transformation steps.
#' @return A formatted life table dataframe.
#' @examples
#' 
#' \dontrun{
#' format_lifetable(data,
#'                   names = c("age" = "age",
#'                           "year" = "yr",
#'                           "interval" = "period",
#'                           "expected"="Expected_Interval"))
#'
#' }
#' @seealso [format_incidence()] The analogous function that formats incidence data
#' @seealso [format_survival()] The analogous function that formats survival data
#' @export

format_lifetable <- function(data, # A dataframe of counts for each unique combination of age and year
                             ages=NULL,                # A vector of ages to be included in the output
                             years=NULL,               # A vector of years to be included in the output
                             names = c("age" = "age",
                                       "year" = "yr",
                                       "interval" = "period",
                                       "expected"="Expected_Interval") # A vector of names containing 1) age, 2) year, 3) interval, and 4)  expected survival, of the form list("age" = ..., "year" = ..., etc.)
)  {
  year <- interval  <-  expected  <- age  <- period  <- agePrev <- yrPrev  <-  NULL
  `%>%` <- dplyr::`%>%`
  
  new <- data.frame(age = data[[names[["age"]]]],
                    year = data[[names[["year"]]]],
                    period = data[[names[["interval"]]]],
                    expected = data[[names[["expected"]]]]) %>%
    dplyr::mutate_all(as.numeric)
  
  
  life.table <- new %>%
    dplyr::filter(!grepl("-",year) & !grepl("-",period) & period != "0 years" & period != "Time 0") %>%
    dplyr::mutate( yrPrev=year+period,
                   agePrev=age+period,
                   expected=dplyr::case_when(any(expected>1)~ as.numeric(expected)/100,
                                             agePrev >=100~0,
                                             TRUE~as.numeric(expected))) %>%
    dplyr::select(agePrev,yrPrev,period,expected)
  
  return(as.data.frame(life.table))
}


#' Format incidence data
#'
#' @description This function formats incidence data for ease of use with functions included in this package. It takes new cases and summarizes them by age and year of diagnosis.
#' @param data Incidence dataframe
#' @param ages Numeric vector. If not included, defaults to all ages in data.
#' @param years Numeric vector. If not specified, defaults to all years in data.
#' @param names Named character vector
#' @param keepExtraCols Logical
#' @details The prevEst function requires properly formatted incidence and survival data. This function, the counterpart to [format_survival()], is designed 
#' to take SEER-like incidence data and format it for the [prevEst()] function. Three columns are necessary: 1) age at diagnosis, 2) year of diagnosis, 
#' and 3) the reported incidence for that combination of the two. While these functions are not mandatory, they help prepare the data for [prevEst()] with simple transformation steps.
#' @return A formatted incidence dataframe.
#' @examples
#' 
#' data(incidence)
#' format_incidence(data=incidence,
#'                   ages = 0:85,
#'                   years = c(2010:2018),
#'                   names = c("ageDiag" = "ageDiag",
#'                           "yrDiag" = "yrDiag",
#'                           "incidence" = "count"),
#'                            keepExtraCols=FALSE)
#'
#'
#' @seealso [format_survival()] The analogous function that formats survival data
#' @export

format_incidence <- function(data,                # A dataframe of counts for each unique combination of age and year
                             ages=NULL,                # A vector of ages to be included in the output
                             years=NULL,               # A vector of years to be included in the output
                             names = c("ageDiag" = "age",
                                       "yrDiag" = "year",
                                       "incidence" = "count"), # A vector of names containing 1) age, 2) year, and 3) counts, of the form list("age" = ..., "year" = ..., etc.)
                             keepExtraCols=FALSE
)  {
  ageDiag <- yrDiag <-  inc.y <- inc.x <-  NULL
  
  `%>%` <- dplyr::`%>%`
  
  options(dplyr.summarise.inform = FALSE)
  
  new <- data.frame(ageDiag = data[[names[["ageDiag"]]]],
                    yrDiag = as.numeric(data[[names[["yrDiag"]]]]),
                    inc = as.numeric(data[[names[["incidence"]]]])) %>%
    dplyr::mutate_all(as.numeric)
  
  if( keepExtraCols==TRUE) {
    new <- new %>%
      dplyr::bind_cols(data %>% dplyr::select(-names))
  }
  
  if (is.null(ages)) {
    ages <- min(new$ageDiag):max(new$ageDiag)
  }
  if ( is.null(years)) {
    years <- min(new$yrDiag):max(new$yrDiag)
  }
  
  skeleton <- tidyr::expand_grid(ageDiag = ages,
                                 yrDiag =  as.numeric(years),
                                 inc = as.numeric(0)) %>%
    dplyr::arrange(ageDiag, yrDiag)
  
  final <-  dplyr::left_join(skeleton, new, by = c("ageDiag", "yrDiag")) %>%
    dplyr::mutate(count = ifelse(inc.y == 0, inc.x, inc.y)) %>%
    dplyr::select(-c(inc.y, inc.x)) %>%
    dplyr::arrange(ageDiag, yrDiag)
  
  return(as.data.frame(final))
}