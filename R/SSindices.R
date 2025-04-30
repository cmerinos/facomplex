#' @details
#' This function builds on the logic of \code{lazy.fa::ss_index}, which evaluates off-diagonal 
#' complexity based on squared loadings. \code{SSindices()} extends the concept by incorporating 
#' a user-defined binary target structure, allowing explicit evaluation of how well the factor 
#' solution conforms to theoretical expectations.
#'
#' \strong{Interpretation of the indices:}
#' \itemize{
#'   \item \code{SStarget}: Proportion of total variance that is aligned with the expected structure. 
#'   Higher values indicate clearer factor-item alignment.
#'   \item \code{SSntarget}: Proportion of variance explained by unexpected (non-target or cross) loadings. 
#'   Reflects the degree of noise or factorial complexity in the solution.
#'   \item \code{SSratio}: The ratio between expected and non-expected variance.
#'         It indicates how dominant the expected structure is over residual complexity.
#' }
#'
#' \strong{Interpreting SSntarget (cross-loading contribution):}
#' \itemize{
#'   \item \code{≈ 0.00}: Perfectly simple structure (each item loads clearly on only one factor).
#'   \item \code{< 0.05}: Very good factor differentiation.
#'   \item \code{0.05 – 0.15}: Moderate cross-loading complexity.
#'   \item \code{> 0.15}: Substantial interdependence or noise across factors.
#' }
#'
#' \strong{Interpreting SSratio:}
#' \itemize{
#'   \item \code{> 4}: Excellent structure – target pattern clearly dominates.
#'   \item \code{2 – 4}: Good structure with acceptable noise.
#'   \item \code{1 – 2}: Target and cross-loadings are comparable – caution advised.
#'   \item \code{≈ 1}: Equal contribution – borderline structure.
#'   \item \code{< 1}: Cross-loadings dominate – weak or misaligned structure.
#' }
#' @examples
#' L <- matrix(c(
#'   0.6, 0.2,
#'   0.5, 0.3,
#'   0.1, 0.7
#' ), nrow = 3, byrow = TRUE)
#'
#' T <- matrix(c(
#'   1, 0,
#'   1, 0,
#'   0, 1
#' ), nrow = 3, byrow = TRUE)
#'
#' SSindices(L, T)
#'
#' @export
SSindices <- function(loadings, target) {
  L <- as.matrix(loadings)
  T <- as.matrix(target)
  
  if (!all(dim(L) == dim(T))) {
    stop("Dimensions of 'loadings' and 'target' must match.")
  }
  
  L2 <- L^2
  total_var <- sum(L2)
  var_target <- sum(L2 * T)
  var_nontarget <- sum(L2 * (1 - T))
  
  SStarget <- var_target / total_var
  SSntarget <- var_nontarget / total_var
  SSratio <- if (SSntarget == 0) Inf else SStarget / SSntarget
  
  result <- data.frame(
    SStarget = round(SStarget, 4),
    SSntarget = round(SSntarget, 4),
    SSratio = round(SSratio, 4)
  )
  
  return(result)
}
