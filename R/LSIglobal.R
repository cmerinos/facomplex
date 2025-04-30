#' LSIglobal: Loading Simplicity Index (Lorenzo-Seva, 2003)
#'
#' Computes the Loading Simplicity Index (LSI) as proposed by Lorenzo-Seva (2003),
#' adapted from the implementation in \code{lazy.fa::LS_index}. This global index 
#' evaluates the overall factorial simplicity of a loading matrix using a non-linear 
#' weighting scheme to emphasize dominant factor loadings.
#'
#' @param loadings A numeric matrix or data frame of factor loadings. Rows represent items,
#' columns represent factors.
#'
#' @return A numeric value between 0 and 1 indicating the global simplicity of the solution.
#'
#' @details
#' The LSI reflects the extent to which the factor solution exhibits simple structure.
#' It applies a double normalization to the loading matrix and then computes a non-linear
#' function over the squared normalized loadings. High values (close to 1) indicate 
#' greater factorial simplicity, while lower values (close to 0) reflect more diffuse or 
#' complex loading patterns.
#'
#' The index is scaled to the interval [0, 1] as follows:
#' \deqn{
#' LSI = \frac{w - e}{1 - e}
#' }
#' where \eqn{w} is the weighted average complexity across all loadings, and \eqn{e} is
#' the theoretical minimum expected under uniform distribution of loadings.
#'
#' @references
#' Lorenzo-Seva, U. (2003). A factor simplicity index. \emph{Psychometrika}, 68(1), 49–60. 
#' \doi{10.1007/BF02296652}
#'
#' Code adapted from: \code{lazy.fa::LS_index}
#'
#' @examples
#' L <- matrix(c(
#'   0.6, 0.2,
#'   0.5, 0.3,
#'   0.1, 0.7
#' ), nrow = 3, byrow = TRUE)
#' LSIglobal(L)
#'
#' @export
LSIglobal <- function(loadings) {
  L <- as.matrix(loadings)
  p <- nrow(L)
  r <- ncol(L)
  
  # Step 1: Normalization matrices
  C <- diag(diag(t(L) %*% L))              # column sums of squares
  H <- diag(diag(L %*% diag(1/diag(C)) %*% t(L)))  # row-wise norms
  
  # Step 2: Double-normalize and square
  B2 <- (diag(1 / sqrt(diag(H))) %*% L %*% diag(1 / sqrt(diag(C))))^2
  
  # Step 3: Weighted function of normalized squared loadings
  epsilon <- 1e-6
  w <- sum(apply(B2 + epsilon, 1:2, function(x) x^(10 * (x - epsilon)))) / (p * r)
  
  # Step 4: Minimum theoretical value under uniformity
  e <- (1 / r + epsilon)^(10 / r)
  
  # Step 5: Final index
  LSI <- (w - e) / (1 - e)
  return(round(LSI, 4))
}
