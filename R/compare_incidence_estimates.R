#' Visualize incidence estimates from multiple datasets
#'
#' @description Generates a visual comparison of multiple incidence estimates by year. Requires properly formated incidence estimates.
#' @param incidence1 Incidence dataframe for primary incidence data set (required).
#' @param incidence2 Incidence dataframe for dataset used to estimate complete incidence (optional).
#' @param complete_incidence Incidence dataframe for complete incidence estimated using both datasets (optional).
#' @param projected_incidence Incidence dataframe for projected incidence (optional).
#' @param names Named character vector
#' @details This function requires at least one incidence estimate. It will generate a ggplot comparing up to 4 incidence estimates from a main data set, comparison dataset, complete incidence, and project incidence files
#' @examples
#' 
#'  \dontrun{
#' data(incidence)
#' incidence1 <- format_incidence(data=incidence,
#'                   ages = 0:85,
#'                   years = c(2010:2018),
#'                   names = c("ageDiag" = "ageDiag",
#'                           "yrDiag" = "yrDiag",
#'                           "incidence" = "count"),
#'                            keepExtraCols=FALSE)
#'                            
#' data(incidence_est)
#' incidence2 <- format_incidence(data=incidence_est,
#'                   ages = 0:85,
#'                   years = c(2010:2018),
#'                   names = c("ageDiag" = "ageDiag",
#'                           "yrDiag" = "yrDiag",
#'                           "incidence" = "count"),
#'                            keepExtraCols=FALSE)
#'                            
#' complete_incidence <- regPrev(incidence=incidence1,
#'                   incidence_est=incidence2,
#'                   regYr=2004:2021,
#'                   durationYr=1975:2009 )
#'                   
#' projected_incidence <- project_incidence(data=complete_incidence,
#'                   projection.years=2022:2025)                   
#' 
#' compare_incidence_estimates(incidence1=incidence1,
#'                   incidence2=incidence2,
#'                   complete_incidence=complete_incidence,
#'                   projected_incidence=projected_incidence,
#'                   names=c("incidence1"="incidence1",
#'                           "incidence2"="incidence2",
#'                           "projected_incidence"="Projected incidence",
#'                           "complete_incidence"="Complete incidence")) 
#'}
#' @export


compare_incidence_estimates <- function(incidence1=NULL,
                                        incidence2=NULL,
                                        complete_incidence=NULL,
                                        projected_incidence=NULL,
                                        names=c("incidence1"="incidence1",
                                                "incidence2"="incidence2",
                                                "projected_incidence"="Projected incidence",
                                                "complete_incidence"="Complete incidence")) {
  
  `%>%` <- dplyr::`%>%`
  aes <- ggplot2::aes
  
  count <- yrDiag <-  NULL
  
  if(is.null(incidence1)){
    stop("Primary incidence value (incidence1) must be provided for comparison \n")
  } else {
    
    incidence1 <- incidence1 %>%
      dplyr::group_by(yrDiag)%>%
      dplyr::summarize(count=sum(count))
    
    figure <- ggplot2::ggplot() +
      ggplot2::geom_line(data=incidence1, aes(x=yrDiag,y=count,color=names[["incidence1"]])) 
    
    if(!is.null(incidence2)) {
      
      incidence2 <- incidence2 %>%
        dplyr::group_by(yrDiag)%>%
        dplyr::summarize(count=sum(count))
      
      figure <- figure +
        ggplot2::geom_line(data=incidence2, aes(x=yrDiag,y=count,color=names[["incidence2"]])) 
    }
    if(!is.null(projected_incidence)) {
      
      projected_incidence <- projected_incidence %>%
        dplyr::group_by(yrDiag)%>%
        dplyr::summarize(count=sum(count)) %>%
        dplyr::filter(yrDiag > max(incidence1$yrDiag))
      
      figure <- figure +
        ggplot2::geom_line(data=projected_incidence, aes(x=yrDiag,y=count,color=names[["projected_incidence"]])) 
    }
    if(!is.null(complete_incidence)) {
      
      complete_incidence <- complete_incidence %>%
        dplyr::group_by(yrDiag)%>%
        dplyr::summarize(count=sum(count)) %>%
        dplyr::filter(yrDiag < min(incidence1$yrDiag))
      
      figure <- figure +
        ggplot2::geom_line(data=complete_incidence, aes(x=yrDiag,y=count,color=names[["complete_incidence"]])) 
    }
    
    figure <- figure +
      ggplot2::scale_y_continuous(name="Incidence count",label=scales::comma, expand=ggplot2::expansion(mult=c(0,.2))) +
      ggplot2::scale_x_continuous(name="Year of diagnosis") +
      ggplot2::scale_color_ordinal(name="Estimates") +
      ggplot2::theme_minimal()
    return(figure)
  }
}
  
  
  