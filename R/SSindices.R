#' SSindices: Target-Based Simple Structure Indices
#'
#' Computes three target-based indices of factorial simplicity:
#' \code{SStarget}, \code{SSntarget}, and their ratio \code{SSratio}.
#' These quantify how much of the total explained variance is aligned 
#' with a predefined target structure versus misaligned (cross-loading) variance.
#'
#' @param loadings A numeric matrix or data frame of factor loadings (items × factors).
#' @param target A binary matrix or data frame of the same dimensions as \code{loadings}, 
#' indicating the expected loading structure: 1 = expected (target) loading, 0 = non-target.
#' @param per.factor Logical. If TRUE, returns a data frame with indices computed per factor (column). Default is FALSE.
#'
#' @return A data.frame with:
#' \itemize{
#'   \item \code{SStarget}: Proportion of total explained variance due to target loadings.
#'   \item \code{SSntarget}: Proportion of variance explained by non-target (cross) loadings.
#'   \item \code{SSratio}:The ratio between target and non-target variance (\code{SStarget / SSntarget}). Values >1 indicate dominance of the expected structure.
#' }
#'
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
#'
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
#' SSindices(L, T, per.factor = TRUE)
#'
#' @references
#' Thurstone, L. L. (1947). *Multiple factor analysis*. University of Chicago Press.
#' 
#' @export
SSindices <- function(loadings, target, per.factor = FALSE) {
  L <- as.matrix(loadings)
  T <- as.matrix(target)
  
  if (!all(dim(L) == dim(T))) {
    stop("Dimensions of 'loadings' and 'target' must match.")
  }
  
  L2 <- L^2
  
  if (isTRUE(per.factor)) {
    results <- sapply(seq_len(ncol(L)), function(j) {
      total_var_j <- sum(L2[, j])
      if (total_var_j == 0) {
        c(SStarget = NA, SSntarget = NA, SSratio = NA)
      } else {
        var_target_j <- sum(L2[, j] * T[, j])
        var_nontarget_j <- sum(L2[, j] * (1 - T[, j]))
        SStarget_j <- var_target_j / total_var_j
        SSntarget_j <- var_nontarget_j / total_var_j
        SSratio_j <- if (SSntarget_j == 0) Inf else SStarget_j / SSntarget_j
        c(SStarget = round(SStarget_j, 4),
          SSntarget = round(SSntarget_j, 4),
          SSratio = round(SSratio_j, 4))
      }
    })
    return(as.data.frame(t(results), row.names = paste0("Factor", seq_len(ncol(L)))))
  } else {
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
}
