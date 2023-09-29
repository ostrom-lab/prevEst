#' Format life table data
#'
#' @description This function formats life table data for ease of use with functions included in this package. 
#' @param data Incidence dataframe
#' @param names Named character vector
#' @details Survival projection functions in prevEst  requires properly formatted life table data. This function, the counterpart to [format_incidence()] and [format_survival()], is designed 
#' to take SEER-like life table data and format it for the [prevEst()] function. Four columns are necessary: 1) age, 2) year, 3) survival interval (in years), 
#' and 4) expected interval survival. While these functions are not mandatory, they help prepare the data for [prevEst()] with simple transformation steps.
#' @return A formatted life table dataframe.
#' @examples
#' 
#' format_lifetable(life.table.raw,
#'                   names = c("age" = "age",
#'                           "year" = "yr",
#'                           "interval" = "period",
#'                           "Expected"="Expected_Interval"))
#'
#'
#' @seealso [format_incidence()] The analogous function that formats incidence data
#' @seealso [format_survival()] The analogous function that formats survival data
#' @export

format_lifetable <- function(data, # A dataframe of counts for each unique combination of age and year
                             names = c("age" = "age",
                                       "year" = "yr",
                                       "interval" = "period",
                                       "Expected"="Expected_Interval") # A vector of names containing 1) age, 2) year, 3) interval, and 4)  expected survival, of the form list("age" = ..., "year" = ..., etc.)
)  {
  
  `%>%` <- dplyr::`%>%`
  
  options(dplyr.summarise.inform = FALSE)
  
  life.tables <- data %>%
    dplyr::rename(c("age"= names[["age"]],
             "year" = names[["year"]],
             "interval" =  names[["interval"]],
             "Expected"= names[["Expected"]])) %>%
    dplyr::filter(!grepl("-",year) & !grepl("-",interval) & interval != "0 years" & interval != "Time 0" & !grepl("mon",interval)) %>%
    dplyr::mutate(Expected=ifelse(any(Expected>1),as.numeric(Expected)/100, as.numeric(Expected)),
           age=as.numeric(gsub("[^0-9.-]","",age)),
           period=as.numeric(gsub("[^0-9.-]","",interval)),
           year=as.numeric(year)) %>%
    dplyr::mutate(yrPrev=year+period,
           agePrev=age+period,
           Expected=ifelse(agePrev >=100,0,Expected )) %>%
    dplyr::select(agePrev,yrPrev,period,Expected)
  
  return(as.data.frame(life.tables))
}


