#' @title LSI - Loading Simplicity Index (Lorenzo-Seva, 2003)
#' @description Computes the global Loading Simplicity Index (LSI) proposed by Lorenzo-Seva (2003), based on a matrix of factor loadings.
#'
#' @param loadings A matrix or data frame of factor loadings. Rows represent items, columns represent factors.
#'
#' @return A single numeric value representing the global simplicity index.
#'
#' @details
#' The Loading Simplicity Index (LSI) is a global measure of factor simplicity that captures how clearly 
#' items load on a single factor across the entire loading matrix. The LSI captures the extent to which 
#' each item's communality is concentrated on a single factor, emphasizing structural clarity across the matrix.
#' The index applies a nonlinear weighting function to emphasize dominant loadings while down-weighting smaller ones.
#' LSI ranges from 1 (perfectly simple structure) to 0 (fully complex structure). The index is designed to be 
#' scale-invariant and interpretable regardless of the number of factors or items in the matrix.
#'
#' The LSI is computed as:
#'
#' \deqn{LSI = \frac{w - e}{1 - e}}
#'
#' where \eqn{w} is the mean of weighted item complexity values, and \eqn{e} is the theoretical minimum 
#' value of \eqn{w}, used for normalization. The result is scaled between 0 and 1, with higher values indicating 
#' simpler structures.
#'
#' **Note:** Although the computation involves intermediate values per item (\eqn{w_i}), these should not 
#' be interpreted individually. The LSI is validated and interpretable only as a global index.
#'
#' @references
#' Lorenzo-Seva, U. (2003). A factor simplicity index. *Psychometrika, 68*(1), 49–60.
#' https://doi.org/10.1007/BF02296652
#'
#' @seealso \code{\link{BSI}}, \code{\link{FSI}}, \code{\link{plot_simplicity}} for related indices and visualizations.
#'
#' @examples
#' ex1_data <- data.frame(
#'   F1 = c(0.536, 0.708, 0.600, 0.673, 0.767, 0.481, -0.177, 0.209, -0.097, -0.115, 0.047, 0.024),
#'   F2 = c(-0.110, 0.026, 0.076, 0.011, -0.160, 0.106, 0.668, 0.438, 0.809, 0.167, 0.128, 0.041),
#'   F3 = c(-0.100, 0.036, 0.086, 0.021, -0.150, 0.116, 0.678, 0.448, 0.819, 0.577, 0.738, 0.751)
#' )
#' 
#' lsi <- LSI(ex1_data)
#' cat("Global LSI:", lsi, "\n")
#'
#'@seealso
#'\code{\link{BSI}} for Bentler’s index, \code{\link{FSI}} for Fleming’s index, and \code{\link{plot_simplicity}} 
#'for visual inspection of loadings.
#'
#' @author Cesar Merino-Soto
#' 
#' @export
LSI <- function(loadings) {
  if (!is.matrix(loadings) && !is.data.frame(loadings)) {
    stop("The argument 'loadings' must be a matrix or data frame.")
  }
  
  L <- as.matrix(loadings)
  p <- nrow(L)
  r <- ncol(L)
  epsilon <- 1e-6
  
  # Paso 1: Normalización por columnas
  col_norms <- sqrt(colSums(L^2))
  if (any(col_norms == 0)) stop("One or more columns have zero norm (all-zero loadings).")
  Lc <- sweep(L, 2, col_norms, FUN = "/")
  
  # Paso 2: Normalización por filas
  row_norms <- sqrt(rowSums(Lc^2))
  row_norms[row_norms == 0] <- 1
  B <- sweep(Lc, 1, row_norms, FUN = "/")
  
  # Paso 3: Cálculo de w
  w <- mean(apply(B, 1, function(row) {
    sum((row^2 + epsilon) * 10^(row^2)) / r
  }))
  
  # Paso 4: Valor mínimo teórico e
  bij <- rep(1 / sqrt(r), r)
  e <- sum((bij^2 + epsilon) * 10^(bij^2)) / r
  
  # Paso 5: Índice final
  LSI_val <- (w - e) / (1 - e)
  LSI_val <- max(min(LSI_val, 1), 0)  # Forzar al rango [0,1]
  
  return(round(LSI_val, 4))
}
