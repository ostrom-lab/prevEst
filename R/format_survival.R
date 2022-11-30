#' Format survival data
#' @description This function formats survival data for streamlined use with functions included in this package. While designed to work with case-listing, it can also work with already summarized survival data provided by the SEER*Stat "Survival Session".
#'
#' @param data Survival dataframe
#' @param ages Numeric vector
#' @param years Numeric vector
#' @param prevYear Numeric vector
#' @param assumption Named character vector
#' @param life.table Dataframe of SEER life table
#' @param names Named vector
#' @param keepExtraCols Logical
#' @details The prevEst function requires properly formatted incidence and survival data. This function, the counterpart to [format_incidence()],is designed 
#' to take SEER-like incidence data and format it for the [prevEst()] function. Three columns are necessary: 1) age at diagnosis, 2) year of diagnosis, 
#' and 3) the observed survival for the combination of the two. While these functions are not mandatory, they help prepare the data for [prevEst()] with simple transformation steps.
#' @return A formatted survival dataframe.
#'
#' @examples
#' 
#' data(survival)
#' data(life.table)
#' 
#' format_survival(survival,
#'                 ages = c(0:85),
#'                 years = c(2015:2018),
#'                 assumption = "nosurvival",
#'                 names = c("ageDiag" = "ageDiag",
#'                           "yrDiag" = "yrDiag",
#'                           "Observed" = "survival"),
#'                 keepExtraCols = FALSE)
#'                 
#' format_survival(survival,
#'                 ages = c(0:85),
#'                 years = c(1995:2018),
#'                 assumption = "population",
#'                 names = c("ageDiag" = "ageDiag",
#'                           "yrDiag" = "yrDiag",
#'                           "Observed" = "survival"),
#'                 life.table = life.table,
#'                 keepExtraCols = FALSE)
#' 
#'
#'
#'
#' @seealso [prevEst::format_incidence()] The analogous function that formats incidence data
#' @export

format_survival <- function(data, # Survival data to be formatted
                            ages = NULL, # Ages included in the data to be included in the output
                            years = NULL, # Years included in the data to be included in the output
                            prevYear = NULL, # Year to calculate prevalence for. Defaults to the highest years in data.
                            assumption="nosurvival", 
                            life.table=NULL,
                            names=c("ageDiag"="ageDiag",
                                    "yrDiag"="yrDiag",
                                    "Observed"="Observed"),
                            keepExtraCols=FALSE
) {
  options(dplyr.summarise.inform = FALSE)
  `%>%` <- dplyr::`%>%`
  
  if(is.null(years)) {
    years = sort(unique(data[[names[["yrDiag"]]]]))
  }
  if(is.null(prevYear)) {
    prevYear = max(years)
  } 

  new <- data.frame(ageDiag  = as.numeric(gsub("\\D", "", data[[names[["ageDiag"]]]])),
                    yrDiag = as.numeric(gsub("\\D", "", data[[names[["yrDiag"]]]])),
                    survival = as.numeric(data[[names[["Observed"]]]])) %>%
    dplyr::filter(yrDiag %in% years) %>%
    dplyr::mutate(yrPrev = prevYear,
           period = yrPrev-yrDiag,
           agePrev = ageDiag+period,
           survival = dplyr::case_when(survival>1 ~ survival/100,
                                TRUE ~ survival)) %>%
    dplyr::arrange(yrDiag, ageDiag)
  
  if(keepExtraCols==TRUE) {
    new <- new %>%
      dplyr::bind_cols(data %>% dplyr::select(-names))
  }
  
  years.observed.surv = length(unique(new$yrDiag))
  
  # Create skeleton dataframe and left join to "new" dataframe to handle missing values.
  skeleton <- tidyr::expand_grid(ageDiag  = ages,
                                 yrDiag = years,
                                 yrPrev = prevYear) %>%
    dplyr::mutate(period=yrPrev-yrDiag,
                  agePrev=ageDiag+period) %>%
    dplyr::arrange(ageDiag , yrDiag)
  
  if (assumption == "population") {
    if (is.null(life.table)) {
      stop("Life table must be provided for population survival \n")
    } else {
      message("Applying population-level survival \n")
    }
    if(any(!names(life.table) %in% c("period", "ageDiag", "expected"))){
      stop("Life tables must contain 'period', 'ageDiag', and 'expected' columns \n")
    }
    
    life.table <- life.table %>% 
      dplyr::mutate_all(as.numeric) %>%
      dplyr::select(period, ageDiag, expected) %>%
      dplyr::arrange(desc(period)) %>%
      dplyr::filter(period <= length(years) & 
             ageDiag %in% ages)
    
    full.survival.temp1 <- skeleton  %>%
      dplyr::left_join(new, by=names(skeleton)) %>%
      dplyr::mutate_all(as.numeric) %>%
      dplyr::left_join(life.table %>% dplyr::mutate_all(as.numeric), by = c("ageDiag", "period"))  %>%
      dplyr::mutate(expected = dplyr::case_when(agePrev>=100 ~ 0,
                                         TRUE~expected),
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
      dplyr::bind_rows(full.survival.temp2 %>% dplyr::filter(period > years.observed.surv)) %>%
      dplyr::arrange(ageDiag, period) %>%
      dplyr::select(-expected)

  } else if (assumption=="nosurvival") {
    message("Applying no survival assumption \n")
    full.survival <- skeleton  %>%
      dplyr::left_join(new, by = names(skeleton)) %>%
      dplyr::mutate(survival = dplyr::case_when(period > years.observed.surv ~ 0, TRUE ~ survival))
    
  } else if (assumption=="fill") {
    
    message("Filling survival \n")
    full.survival <- skeleton %>%
      dplyr::left_join(new, by = names(skeleton)) %>%
      dplyr::group_by(ageDiag) %>%
      dplyr::arrange(ageDiag, period) %>%
      tidyr::fill(survival, .direction = "downup") %>%
      dplyr::ungroup()
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