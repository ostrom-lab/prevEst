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
                    inc = as.numeric(data[[names[["incidence"]]]])) 
  
  if( keepExtraCols==TRUE) {
    new <- new %>%
      dplyr::bind_cols(data %>% dplyr::select(-names))
  }
  
  if ( is.null(ages)) {
    ages <- min(new$ageDiag):max(new$ageDiag)
  }
  if ( is.null(years)) {
    ages <- min(new$yrDiag):max(new$yrDiag)
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
