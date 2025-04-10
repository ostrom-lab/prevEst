#' Estimate complete incidence
#'
#' @param incidence Incomplete incidence dataframe containing age at diagnosis (named "ageDiag"), year of diagnosis (named "yrDiag"), and count of new cases (named "count)
#' @param incidence_est Estimated incidence dataframe for smaller population area (with longer follow-up) containing all years with age at diagnosis (named "ageDiag"), year of diagnosis (named "yrDiag"), and count of new cases (named "count)
#' @param regYr Years to do regression on i.e. c(2000:2010). If not specified, will default to all overlapping years in both datasets.
#' @param durationYr Years that require estimates from regression. If not specified, will default to years present in only estimated incidence data frame.
#' @return An incidence dataframe for the years provided.
#' @examples
#' 
#' data(incidence)
#' data(incidence_est)
#' 
#' regPrev(incidence = incidence,
#'         incidence_est = incidence_est,
#'        regYr = c(2001:2017),
#'        durationYr = c(1975:2000))
#'
#' @export

regPrev <- function(
    incidence,
    incidence_est,
    regYr = NULL,
    durationYr = NULL
){
  ageDiag <- yrDiag <- data <- model <- predicted_incidence <- count.x <- count_pred <- count <- ageDiag <- period <- NULL
  
  `%>%` <- dplyr::`%>%`
  
  incidence <- incidence %>%
    dplyr::select(c("ageDiag", "yrDiag","count")) %>%
    dplyr::mutate_all(as.numeric)
  
  incidence_est <- incidence_est %>%
    dplyr::select(c("ageDiag", "yrDiag","count")) %>%
    dplyr::mutate_all(as.numeric)
  
  idf <- dplyr::full_join(incidence, incidence_est, by = c("ageDiag", "yrDiag"))
  
  if(is.null(durationYr) & is.null(regYr)){
    message("Guessing durationYr and regYr as they are not specified. \n")
    # Guess years for regression
    regYr <- intersect(unique(incidence$yrDiag),unique(incidence_est$yrDiag))
    durationYr <- setdiff(unique(incidence_est$yrDiag),regYr)
  }
  # Linear regression and resulting diagnostic statistics
  regprev <- idf %>%
    dplyr::arrange(ageDiag, yrDiag) %>%
    dplyr::group_by(ageDiag) %>%
    tidyr::nest() %>%
    dplyr::mutate(data = purrr::map(data, function(x) x %>% dplyr::mutate(count.y = dplyr::case_when(all(is.na(count.y)) ~ 0,
                                                                                                     T ~ count.y),
                                                                          count.x = dplyr::case_when(all(is.na(count.x)) & all(is.na(count.y)) ~ 0,
                                                                                                     all(is.na(count.x)) ~ count.y,
                                                                                                     T ~ count.x))),
                  model = purrr::map(data, function(x) stats::lm(count.x ~ count.y, data = x %>% dplyr::filter(yrDiag %in% regYr))),
                  predicted_incidence = purrr::map2(data,
                                                    model,
                                                    ~modelr::add_predictions(data = as.data.frame(.x  %>% dplyr::filter(yrDiag %in% durationYr)),
                                                                             model = .y,
                                                                             var = "count_pred"))) %>%
    dplyr::select(-c(data, model)) %>%
    tidyr::unnest(cols=predicted_incidence) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(count = ifelse(is.na(count.x), count_pred, count.x)) %>%
    dplyr::select(ageDiag, yrDiag, count) %>%
    dplyr::bind_rows(idf  %>% dplyr::filter(yrDiag %in% regYr) %>% dplyr::rename(c("count"="count.x")) %>%  dplyr::select(ageDiag, yrDiag, count)) %>%
    dplyr::arrange(ageDiag, yrDiag) %>%
    dplyr::mutate(count = ifelse(as.numeric(count) <= 0, 0, round(count)))
  
  return(regprev)
}
