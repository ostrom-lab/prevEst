#' Apply population survival assumption for years after observation
#' @description This function works within a survival formatting function to have survival go to population expected survival after end of observed survival ye
#' @export

population_survival <- function(life.table=life.table, survival.data=survival.data,years.observed.surv=years.observed.surv,ages=ages) {
  
  yrPrev <- agePrev <- expected <- period <- survival <- ageDiag <-  desc <- yrDiag <-  NULL
  
  `%>%` <- dplyr::`%>%`
  
  
  life.table.analysis <- life.table %>% 
    dplyr::mutate_all(as.numeric) %>%
    dplyr::filter(period <= years.observed.surv) %>%
    dplyr::filter(agePrev %in% ages) %>%
    dplyr::select(period, agePrev, expected, yrPrev) %>%
    dplyr::arrange(desc(period)) 
  
  full.survival.temp1 <- survival.data  %>%
    dplyr::mutate_all(as.numeric) %>%
    dplyr::left_join(life.table.analysis %>%
                       dplyr::mutate_all(as.numeric), by = c("ageDiag"="agePrev", "period","yrPrev"))  %>%
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
    dplyr::bind_rows(full.survival.temp2 %>% 
                       dplyr::filter(period > years.observed.surv)) %>%
    dplyr::arrange(ageDiag, period) %>%
    dplyr::select(-expected)
  
  return(full.survival)
  
}