#' Project survival data for unobserved year(s)
#'
#' @param data Survival dataframe for which prevalence is to be projected
#' @param ages Numeric vector of ages to be included
#' @param observation.years Numeric vector of observed years to build the model on
#' @param projection.years Numeric vector of years to predict survival for
#' @param prevYear Year for which prevalence is being estimated
#' @param assumption Named character, either "population" or "nosurvival"
#' @param life.table Dataframe of SEER life table
#' @param names Named character vector pointing to column names
#' @param keepExtraCols Logical
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
                             ages,
                             observation.years = NULL,               # A vector of years to build the model on
                             projection.years = NULL,
                             prevYear = NULL,
                             assumption="nosurvival",
                             life.table=NULL,
                             names=c("ageDiag"="ageDiag",
                                     "yrPrev"="yrPrev",
                                     "yrDiag"="yrDiag",
                                     "Observed"="observed",
                                     "expected"="Expected"), # A list of names containing 1) age, 2) year, and 3) survival, of the form list("age" = ..., "year" = ..., etc.)
                             keepExtraCols=FALSE
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
  if(is.null(prevYear)) {
    prevYear=max(c(observation.years,projection.years))
  }
  
  
  new <- data.frame(ageDiag = as.numeric(gsub("\\D", "", data[[names[["ageDiag"]]]])),
                    yrPrev = as.numeric(gsub("\\D", "", data[[names[["yrPrev"]]]])),
                    yrDiag = as.numeric(gsub("\\D", "", data[[names[["yrDiag"]]]])),
                    survival = as.numeric(data[[names[["Observed"]]]])) %>%
    dplyr::mutate(period = yrPrev-yrDiag) %>%
    dplyr::distinct(.keep_all=TRUE)
  
  first.year <- min(new$yrDiag)
  final.year <- as.numeric(max(projection.years))
  years.observed.surv = length(observation.years)
  
  if(keepExtraCols==TRUE) {
    new <- new %>%
      dplyr::bind_cols(data %>% dplyr::select(-names))
  }
 if(all(is.na(new$survival)) & is.null(life.table)){
   stop("No survival data provided")
 }
 if(all(is.na(new$survival)) & !is.null(life.table)){
   
   if(any(!c("period", "agePrev", "expected",'yrPrev') %in% names(life.table))){
     stop("Life tables must contain 'period', 'agePrev', 'yrPrev', and 'expected' columns \n")
   }
   
   
    new <- new %>% 
      dplyr::left_join(life.table, by = c("ageDiag"="agePrev", "period","yrPrev")) %>% 
      dplyr::filter(yrDiag <= length(observation.years) & 
             ageDiag %in% ages) %>%
      tidyr::fill(expected, .direction = "downup") %>%
      dplyr::group_by(ageDiag, period) %>%
      dplyr::arrange(ageDiag, period) %>% 
      dplyr::mutate(survival = cumprod(expected)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate_all(as.numeric) %>%
      dplyr::select(-expected)
}
  cat("Projecting ", length(projection.years), " years of survival for ",min(projection.years),"-",max(projection.years), "\n", sep = "")
  
  new <- new  %>%
    tidyr::drop_na() %>%
    dplyr::filter(yrDiag %in% observation.years) %>%
    dplyr::mutate( period=yrPrev-yrDiag,
                   agePrev = ageDiag+period,
                   survival = dplyr::case_when(survival  > 1 ~ survival /100,
                                        TRUE ~ survival))
  proj.surv <- new %>%
    dplyr::group_by(period) %>%
    tidyr::nest() %>%
    dplyr::mutate(predicted_survival=purrr::map(data, ~modelr::add_predictions(data=expand.grid(yrDiag = projection.years,
                                                                                                ageDiag = ages),
                                                                               lm(survival ~ as.numeric(yrDiag) + as.numeric(ageDiag), data = .x),
                                                                               var="survival"))) %>%
    dplyr::select(-data) %>%
    tidyr::unnest(cols=predicted_survival) %>%
    dplyr::mutate(yrPrev=yrDiag+period,
                  agePrev=ageDiag+period,
                  survival=dplyr::case_when(as.numeric(survival)<0~0,
                                     as.numeric(survival)>1~1,
                                     TRUE~as.numeric(survival)))
  
  survival.merged <- dplyr::bind_rows(new, proj.surv) %>%
    dplyr::filter(yrDiag >= first.year) %>% 
    dplyr::mutate_all(as.numeric) %>%
    dplyr::arrange(ageDiag, yrDiag, period)
  
  # Create skeleton dataframe and left join to "new" dataframe to handle missing values. Will default to survival = 1 if data mising.
  skeleton <- tidyr::expand_grid(ageDiag  = ages,
                                 yrDiag = first.year:final.year,
                                 yrPrev = first.year:final.year) %>%
    dplyr::distinct(.keep_all=TRUE) %>%
    dplyr::mutate(period=yrPrev-yrDiag,
                   agePrev=ageDiag+period) %>%
    dplyr::arrange(ageDiag , yrDiag)  %>%
    dplyr::mutate_all(as.numeric) %>%
    dplyr::filter(period >= 0)  
  
  if (assumption=="population") {
    if (is.null(life.table)) {
      stop("Life table must be provided for population survival")
    }
    else {
      
      message("Applying population-level survival \n")
      
      if(any(!c("period", "agePrev", "expected",'yrPrev') %in% names(life.table))){
        stop("Life tables must contain 'period', 'agePrev', 'yrPrev', and 'expected' columns \n")
      }
      
      life.table <- life.table %>% 
        dplyr::mutate_all(as.numeric) %>%
        dplyr::select(period, agePrev, expected, yrPrev) %>%
        dplyr::arrange(desc(period)) %>%
        dplyr::filter(period <= years.observed.surv & 
                        agePrev %in% ages)
      
      full.survival.temp1 <- skeleton  %>%
        dplyr::left_join(new, by=names(skeleton)) %>%
        dplyr::mutate_all(as.numeric) %>%
        dplyr::left_join(life.table %>% dplyr::mutate_all(as.numeric), by = c("ageDiag"="agePrev", "period","yrPrev"))  %>%
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
      
    }
  }
  if (assumption=="nosurvival") {
    cat("Applying no survival assumption")
    full.survival <- skeleton  %>%
      dplyr::left_join(new,by=names(skeleton)) %>%
      dplyr::mutate(survival=dplyr::case_when(period > years.observed.surv ~ 0, TRUE ~ survival))
  }
  
  final <- full.survival %>%
    dplyr::mutate(survival=dplyr::case_when(yrDiag==yrPrev ~ 1,
                                            survival>1~survival/100,
                                     survival<=0~0,
                                     agePrev>=100~0,
                                     TRUE~as.numeric(survival)))  %>%
    dplyr::group_by(ageDiag, yrPrev) %>%
    dplyr::arrange(ageDiag, period, yrPrev) %>%
    tidyr::fill(survival, .direction = "downup") %>%
    dplyr::select(ageDiag,agePrev, yrDiag, yrPrev, period , survival) %>%
    dplyr::filter(period >= 0 & yrPrev==prevYear)
  
  return(as.data.frame(final))
}