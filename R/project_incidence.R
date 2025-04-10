#' Project incidence for unobserved year(s)
#'
#' @param data Incidence dataframe for which incidence is to be projected
#' @param data.years years covered by incidence years data.frame (if not specified will use all years in the data)
#' @param projection.years Numeric vector of years to project incidence for
#' @param method GLM family to be base predictions off of
#' @return Predicted survival proportions for \code{years}
#' @return Projected incidence dataframe including projection years
#' @examples
#' \dontrun{
#' project_incidence(data=complete_incidence,
#' data.years=NULL,
#' projection.years = NULL,
#' method = "poisson")
#'}
#' @seealso [project_survival()] The analogous function that projects survival data
#' @export


project_incidence <- function(data,
                              data.years=NULL,
                              projection.years = NULL,
                              method = "poisson") {

  yrDiag <- ageDiag <- predicted_incidence <- NULL
  
  options(dplyr.summarise.inform = FALSE)
  `%>%` <- dplyr::`%>%`

  # Searches for columns containing age, year, incidence, then renames them to be used later.
  incidence <- data %>%
    dplyr::select(c("ageDiag", "yrDiag","count"))

  if(is.null(data.years)) {
    data.years <- min(incidence$yrDiag):max(incidence$yrDiag)
  }
  
  if(is.null(projection.years)) {
    stop("Please specify years for projections. \n")
  }

  cat("Projecting incidence for",min(projection.years),"-",max(projection.years), "\n")

  proj.inc <- incidence %>%
    dplyr::filter(yrDiag %in% data.years) %>%
    dplyr::group_by(ageDiag  ) %>%
    tidyr::nest()
  
  if(method == "poisson") {
    proj.inc <- proj.inc %>%
    dplyr::mutate(predicted_incidence=purrr::map(data, ~modelr::add_predictions(data=data.frame(yrDiag  = projection.years),
                                                                                glm(count ~ as.numeric(yrDiag) , family=method, data = .x),
                                                                                var="count",type="response"))) 
    }
  if(method == "linear") {
    proj.inc <- proj.inc %>%
      dplyr::mutate(predicted_incidence=purrr::map(data, ~modelr::add_predictions(data=data.frame(yrDiag  = projection.years),
                                                                                  lm(count ~ as.numeric(yrDiag) , data = .x),
                                                                                  var="count",type="response"))) 
  }
  if(method == "zeroinf") {
    proj.inc <- proj.inc %>%
      dplyr::mutate(predicted_incidence=purrr::map(data, ~modelr::add_predictions(data=data.frame(yrDiag  = projection.years),
                                                                                  pscl::zeroinfl(count ~ as.numeric(yrDiag) | 1 , data = .x),
                                                                                  var="count",type="response"))) 
  }
  
  proj.inc <- proj.inc %>%
    dplyr::select(-data) %>%
    tidyr::unnest(cols=predicted_incidence) %>%
    dplyr::mutate(count=dplyr::case_when(as.numeric(count)<0~0,
                                  TRUE~round(as.numeric(count),0))) %>%
    dplyr::ungroup() %>%
    dplyr::filter(yrDiag %in% projection.years & !(yrDiag %in% incidence$yrDiag))

  incidence.merged <-  dplyr::bind_rows(incidence,proj.inc)
}
