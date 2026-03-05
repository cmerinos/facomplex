#' @title BSI - Bentler Simplicity Index
#'
#' @description
#' Computes Bentler's Simplicity Index (BSI) for a loading matrix.
#' This is a scale-free, matrix-level measure of factor simplicity
#' originally proposed by Bentler (1977).
#'
#' @param data A numeric matrix or data frame of factor loadings with
#'   \code{p} rows (items/variables) and \code{r} columns (factors).
#'
#' @return
#' A single numeric value in the interval \code{[0, 1]} representing
#' Bentler's global simplicity index for the full loading matrix.
#'
#' @details
#' Let \eqn{L} be a \eqn{p \times r} loading matrix, and let
#' \eqn{A = [l_{ij}^2]} be the matrix of squared loadings. Define
#'
#' \deqn{D = \mathrm{diag}(A'A)}
#'
#' where \eqn{D} is the diagonal matrix formed from the diagonal elements
#' of \eqn{A'A}. Bentler's Simplicity Index is then:
#'
#' \deqn{BSI = \left| D^{-1/2} A'A D^{-1/2} \right|}
#'
#' where \eqn{|.|} denotes the determinant.
#'
#' The index ranges from 0 to 1. Higher values indicate a simpler factor
#' structure. A value of 1 is obtained when the loading matrix shows
#' perfect factorial simplicity (i.e., each variable is associated with
#' only one factor). Bentler's index is invariant to the scale of the
#' factors.
#'
#' @references
#' Bentler, P. M. (1977). Factor simplicity index and transformations.
#' \emph{Psychometrika, 42}(2), 277--295.
#' https://doi.org/10.1007/BF02294054
#'
#' Fleming, J. S. (2003). Computing measures of simplicity of fit for
#' loadings in factor-analytically derived scales.
#' \emph{Behavior Research Methods, Instruments, & Computers, 35}(4), 520--524.
#' https://doi.org/10.3758/BF03195531
#'
#' Lorenzo-Seva, U. (2003). A factor simplicity index.
#' \emph{Psychometrika, 68}(1), 49--60.
#' https://doi.org/10.1007/BF02296652
#'
#' @examples
#' ex1_data <- data.frame(
#'   F1 = c(0.536, 0.708, 0.600, 0.673, 0.767, 0.481, -0.177, 0.209, -0.097, -0.115, 0.047, 0.024),
#'   F2 = c(-0.110, 0.026, 0.076, 0.011, -0.160, 0.106, 0.668, 0.438, 0.809, 0.167, 0.128, 0.041),
#'   F3 = c(-0.100, 0.036, 0.086, 0.021, -0.150, 0.116, 0.678, 0.448, 0.819, 0.577, 0.738, 0.751)
#' )
#'
#' BSI(ex1_data)
#'
#' @author Cesar Merino-Soto
#' @export
BSI <- function(data) {
  # Validate input
  if (!is.matrix(data) && !is.data.frame(data)) {
    stop("Argument 'data' must be a matrix or data frame.")
  }
  
  L <- as.matrix(data)
  
  if (!is.numeric(L)) {
    stop("All values in 'data' must be numeric factor loadings.")
  }
  
  if (anyNA(L)) {
    warning("Missing values detected in 'data'. They will be replaced by 0.")
    L[is.na(L)] <- 0
  }
  
  # Squared loadings matrix
  A <- L^2
  
  # Cross-product of squared loadings
  AtA <- crossprod(A)
  
  # Diagonal matrix D = diag(A'A)
  d <- diag(AtA)
  
  if (any(d <= 0)) {
    warning("At least one column has zero squared loading sum; BSI is undefined. Returning NA.")
    return(NA_real_)
  }
  
  D_inv_half <- diag(1 / sqrt(d), nrow = length(d), ncol = length(d))
  
  M <- D_inv_half %*% AtA %*% D_inv_half
  
  # Numerical cleanup
  M <- (M + t(M)) / 2
  
  bsi <- det(M)
  
  # Clamp to [0, 1] for numerical stability
  bsi <- max(min(Re(bsi), 1), 0)
  
  return(as.numeric(bsi))
}