#' Create crude and adjusted rates from prevEst output
#'
#' @description The function generates adjusted rates from prevalence output
#'
#' @param prevEst_ouput output from prevEst
#' @param adjust Logical. If TRUE, rates will be age-adjusted
#' @param grouped_ages Logical. If TRUE, multi-year age groups are used for adjustment.
#' @param groups If grouped ages are used, vector of grouped age levels contain the lowest group in each age group is selected
#' @param census.population description
#' @param standard_population description
#'
#' @return A dataframe with prevalence rates by age at prevalence
#'
#' @export

adjust_prevEst <- function( prevEst_ouput=prevest, adjust=F, grouped_ages=T, 
                            groups=seq(0,100,5),
                            max.age=85,
                            census.population=census.population,
                            standard.population=US_2000_standard_population ) {
  if(is.null(census.population)) {
    stop("Please specify census population for rates")
  }
  
  if(grouped_ages == T){
    prevEst_ouput<- prevEst_ouput %>%
      dplyr::mutate(agePrev_groups = as.character(cut(as.numeric(agePrev), groups, include.lowest = F, right = F, labels = groups))) %>%
      dplyr::group_by(agePrev) %>%
      dplyr::summarise(prevalence  = sum(prevalence)) %>%
      dplyr::ungroup()
    
    # modify census population to fit groupings used in adjusted rates
    census.population <- census.population %>%
      dplyr::mutate(age = as.character(cut(as.numeric(age), groups, , include.lowest = F,
                                           right = F, labels = groups))) %>%
      dplyr::group_by(age) %>%
      dplyr::summarise(pop = sum(as.numeric(pop))) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(as.numeric(age))
  if( adjust==T) {
    if(is.null(standard.population)) {
      stop("Please specify standard population for rates")
    }
    
    standard.population <- standard.population %>%
      dplyr::mutate(age = as.character(cut(as.numeric(age), groups, include.lowest = F, right = F, labels = groups))) %>%
      dplyr::group_by(age) %>%
      dplyr::summarise(standard_population = sum(as.numeric(standard_population))) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(as.numeric(age))
    
    adjusted_prevEst_ouput <- prevEst_ouput %>%
      dplyr::mutate(prevalence = ifelse(agePrev >= 85, sum(prevest$prevalence[which(prevest$agePrev >= 85)]), prevalence)) %>%
      dplyr::filter(agePrev <= 85) %>%
      dplyr::left_join(census.population, by = c("agePrev" = "age")) %>%
      dplyr::left_join(standard.population, by = c("agePrev" = "age")) %>%
      dplyr::group_by(agePrev) 
  }
    adjusted_prevEst_ouput <- adjusted_prevEst_ouput %>%
      dplyr::mutate(prevalence = ifelse(agePrev >= max.age, sum(.$prevalence[which(.$agePrev >= max.age)]), prevalence)) %>%
      dplyr::filter(agePrev <= max.age) %>%
      dplyr::left_join(census.population, by = c("agePrev" = "age")) %>%
      dplyr::mutate(crude_rate = prevalence/pop*100000)
    
    if( adjust==T) {
    adjusted_prevEst_ouput <- adjusted_prevEst_ouput %>%
      dplyr::mutate(weights = standard_population/sum(standard_population),
                    adjusted_rate = crude_rate*weights,
                    adjusted_lci = asht::wspoissonTest(prevalence, w = as.numeric(weights)/as.numeric(pop),
                                                       wmtype = "tcz", mult=100000)$conf.int[[1]],
                    adjusted_uci = asht::wspoissonTest(prevalence, w = as.numeric(weights)/as.numeric(pop),
                                                       wmtype = "tcz", mult=100000)$conf.int[[2]]) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(as.numeric(agePrev))}
  }
}
