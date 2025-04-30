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
#'
#' @return A data.frame with:
#' \itemize{
#'   \item \code{SStarget}: Proportion of total variance explained by target-aligned loadings.
#'   \item \code{SSntarget}: Proportion of variance explained by non-target (cross) loadings.
#'   \item \code{SSratio}: The ratio of SStarget to SSntarget. Values >1 indicate dominance of the expected structure.
#' }
#'
#' @details
#' This function complements traditional simplicity indices by directly quantifying how
#' much of the loading matrix adheres to an expected factorial pattern.
#' 
#' \code{SSratio} serves as a comparative indicator:
#' \itemize{
#'   \item \code{> 1} = expected structure dominates,
#'   \item \code{≈ 1} = balanced structure vs. noise,
#'   \item \code{< 1} = cross-loadings explain more variance.
#' }
#'
#' @references
#' Kaiser, H. F. (1974). An index of factorial simplicity. \emph{Psychometrika}, 39(1), 31–36.
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
