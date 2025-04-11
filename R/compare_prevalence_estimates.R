#' Visualize prevalence estimates from multiple methods
#'
#' @description Generates a visual comparison of multiple prevalence estimates.
#' @param prevalence1 Dataframe of prevalence estimates (required).
#' @param prevalence2 Dataframe of prevalence estimates (optional).
#' @param prevalence3 Dataframe of prevalence estimates (optional).
#' @param prevalence4 Dataframe of prevalence estimates (optional).
#' @param names Named character vector
#' @param include_counts Logical. Will print text of prevalence counts above bars. 
#' @details This function requires at least one prevalence estimate. It will generate a ggplot comparing up to 4 prevalence estimates.

#' @examples
#' 
#'compare_prevalence_estimates(prevalence1=prevalence1,
#'                   prevalence2=prevalence2,
#'                   prevalence3=prevalence3,
#'                   prevalence4=prevalence4,
#'                   names=c("prevalence1"="prevalence1",
#'                           "prevalence2"="prevalence2",
#'                           "prevalence3"="prevalence3",
#'                           "prevalence4"="prevalence4"),
#'                           include_counts=FALSE)
#'
#' @export

compare_prevalence_estimates <- function(prevalence1=NULL,
                                         prevalence2=NULL,
                                         prevalence3=NULL,
                                         prevalence4=NULL,
                                         names=c("prevalence1"="prevalence1",
                                                 "prevalence2"="prevalence2",
                                                 "prevalence3"="prevalence3",
                                                 "prevalence4"="prevalence4"),
                                         include_counts=FALSE) {

    prevalence <-  NULL
  
  `%>%` <- dplyr::`%>%`
  aes <- ggplot2::aes
  
  if( is.null(prevalence1) & is.null(prevalence2) & is.null(prevalence3) &is.null(prevalence4)){
    stop("At least one prevalence estimate must be provided")
  } else {
    
    prev1_est <- prevalence1 %>% dplyr::summarize(prevalence=sum(prevalence))
    
    figure <- ggplot2::ggplot() +
      ggplot2::geom_col(aes(x=names[["prevalence1"]],y=prev1_est$prevalence)) 
    
    if(include_counts==TRUE) {
      figure <- figure +
        ggplot2::geom_text(aes(x=names[["prevalence1"]],y=prev1_est$prevalence, label=prettyNum(prev1_est$prevalence, big.mark = ",")), size=3,vjust=-1)
      
    }
    if(!is.null(prevalence2)){
      
      prev2_est <- prevalence2 %>% dplyr::summarize(prevalence=sum(prevalence))
      
      figure <- figure +
        ggplot2::geom_col(aes(x=names[["prevalence2"]],y=prev2_est$prevalence))
      
      if(include_counts==TRUE) {
        figure <- figure +
          ggplot2::geom_text(aes(x=names[["prevalence2"]],y=prev2_est$prevalence, label=prettyNum(prev2_est$prevalence, big.mark = ",")), size=3,vjust=-1)
      }
    }
    if(!is.null(prevalence3)){
      
      prev3_est <- prevalence3 %>% dplyr::summarize(prevalence=sum(prevalence))
      
      figure <- figure +
        ggplot2::geom_col(aes(x=names[["prevalence3"]],y=prev3_est$prevalence))
      
      if(include_counts==TRUE) {
        figure <- figure +
          ggplot2::geom_text(aes(x=names[["prevalence3"]],y=prev3_est$prevalence, label=prettyNum(prev3_est$prevalence, big.mark = ",")), size=3,vjust=-1)
      }
    }
    if(!is.null(prevalence4)){
      
      prev4_est <- prevalence4 %>% dplyr::summarize(prevalence=sum(prevalence))
      
      figure <- figure +
        ggplot2::geom_col(aes(x=names[["prevalence4"]],y=prev4_est$prevalence))
      
      if(include_counts==TRUE) {
        figure <- figure +
          ggplot2::geom_text(aes(x=names[["prevalence4"]],y=prev4_est$prevalence, label=prettyNum(prev4_est$prevalence, big.mark = ",")), size=3,vjust=-1)
      }
    }
    
    figure <- figure +
      ggplot2::scale_y_continuous(name="Prevalence count",label=scales::comma, expand=ggplot2::expansion(mult=c(0,.1))) +
      ggplot2::scale_x_discrete(name="Estimation method") +
      ggplot2::theme_minimal()
    
    return(figure)
  }
}


