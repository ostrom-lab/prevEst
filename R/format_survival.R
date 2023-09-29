#' Format survival data
#' @description This function formats survival data for streamlined use with functions included in this package. While designed to work with case-listing, it can also work with already summarized survival data provided by the SEER*Stat "Survival Session".
#'
#' @param data Survival dataframe
#' @param ages Numeric vector
#' @param years Numeric vector
#' @param prevYear Numeric vector
#' @param Observed.Surv Logical
#' @param Expected.Surv Logical
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
#' 
#' data(survival)
#' data(life.table)
#' 
#' format_survival(survival,
#'                 ages = c(0:85),
#'                 years = c(2015:2018),
#'                 SurvType= c("Observed")
#'                 Observed.Surv = TRUE,
#'                 Expected.Surv = FALSE,
#'                 assumption = "nosurvival",
#'                 years.observed.surv = NULL,
#'                 names = c("ageDiag" = "ageDiag",
#'                           "yrDiag" = "yrDiag",
#'                           "Observed" = "survival",
#'                           "Expected" = "Expected"),
#'                 keepExtraCols = FALSE)
#'                 
#' format_survival(survival,
#'                 ages = c(0:85),
#'                 years = c(1995:2018),
#'                 Observed.Surv = TRUE,
#'                 Expected.Surv = FALSE,
#'                 assumption = "population",
#'                 names = c("ageDiag" = "ageDiag",
#'                           "yrDiag" = "yrDiag",
#'                           "Observed" = "survival",
#'                           "Expected" = "Expected"),
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
                            years.observed.surv = NULL,
                            SurvType = "Observed",
                            life.table=NULL,
                            names=c("ageDiag"="ageDiag",
                                    "yrDiag"="yrDiag",
                                    "Observed"="Observed",
                                    "Expected" ="Expected"),
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
                    yrPrev = as.numeric(gsub("\\D", "", data[[names[["yrPrev"]]]])),
                    survival = as.numeric(data[[names[["Observed"]]]])) %>%
    dplyr::distinct(.keep_all=TRUE) %>%
    dplyr::filter(yrDiag %in% years & yrPrev == prevYear) %>%
    dplyr::mutate(period = yrPrev-yrDiag,
           agePrev = ageDiag+period,
           survival = dplyr::case_when(any(survival) ~ survival/100,
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
  
  if (assumption == "population") {
    if (is.null(life.table)) {
      stop("Life table must be provided for population survival \n")
    } else {
      message("Applying population-level survival \n")
    }
    if(any(!c("period", "agePrev", "Expected") %in% names(life.table))){
      stop("Life tables must contain 'period', 'agePrev', and 'Expected' columns \n")
    }
    
    life.table <- life.table %>% 
      dplyr::mutate_all(as.numeric) %>%
      dplyr::select(period, agePrev, Expected,yrPrev) %>%
      dplyr::arrange(desc(period)) %>%
      dplyr::filter(period <= length(years) & 
                      agePrev %in% ages)
    
    full.survival.temp1 <- skeleton  %>%
      dplyr::left_join(new, by=names(skeleton)) %>%
      dplyr::mutate_all(as.numeric) %>%
      dplyr::left_join(life.table %>% dplyr::mutate_all(as.numeric), by = c("agePrev", "period","yrPrev"))  %>%
      dplyr::mutate(Expected = dplyr::case_when(agePrev>=100 ~ 0,
                                         TRUE~Expected),
                    survival = dplyr::case_when(!is.na(survival)~survival,
                                         TRUE~Expected)) 
    
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
      dplyr::select(-Expected)

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
