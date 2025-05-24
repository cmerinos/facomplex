#' Calculate Hofmann Factor Complexity Index
#'
#' This function calculates the Hofmann complexity index (\code{Choff}) for each factor (column) in a factor loading matrix.
#' The index estimates the factorial complexity at the factor level, indicating how many items contribute significantly to each factor.
#' 
#' The index ranges from 1 (indicating a factor with a single item) to p (the total number of **items** in the factor, indicating the number of items involved in the definition of the factor).
#' The formula for the index is based on summing squared and quartic factor loadings, similar to the original Hofmann (1977) method applied to the factor (column) dimension.
#' In this case, when calculating the complexity at the factor level, we assess how many items significantly contribute to each 
#' factor. In a latent dimension with known structure, the resulting value should be equal or close to the number of items expected in this dimension. Values different from the expected 
#' number of items suggests there are items with significant loadings or items close to or in the hyperplane. As more items contribute to the factor, 
#' the value of **Choff** increases, reaching p (the number of items) when all items significantly contribute to the factor.
#'
#' @param data A \code{data.frame} or \code{matrix} containing the factor loadings. Rows represent items, and columns represent factors in the factorial model.
#'              The values in the matrix should be factor loadings, typically between -0.9999 and 0.9999.
#'
#' @return A \code{data.frame} containing one column:
#' \itemize{
#'   \item \code{Choff}: The Hofman factor complexity index for each factor, ranging from 1 (indicating simplicity) to p (maximum complexity).
#' }
#'
#' @details
#' Hofman’s complexity index for factors evaluates how many items contribute to the definition of each factor. A value close to 1 indicates that a factor is well-defined by only one item (unidimensional), while values closer to p indicate higher complexity.
#' 
#' The formula is based on the sum of squared and quartic factor loadings, and the result is normalized so that higher complexity values suggest factors defined by multiple items.
#'
#' @examples
#' # Example factor loading matrix
#' ex1.data <- data.frame(
#'   F1 = c(0.536, 0.708, 0.600, 0.673, 0.767, 0.481, -0.177, 0.209, -0.097, -0.115, 0.047, 0.024),
#'   F2 = c(-0.11, 0.026, 0.076, 0.011, -0.16, 0.106, 0.668, 0.438, 0.809, 0.167, 0.128, 0.041),
#'   F3 = c(-0.1, 0.036, 0.086, 0.021, -0.15, 0.116, 0.678, 0.448, 0.819, 0.577, 0.738, 0.751)
#' )
#' 
#' # Calculate Hofman factor complexity
#' results_factor <- HofmannFac(ex1.data)
#' 
#' # View results
#' print(results_factor)
#'
#' @references
#' Hofmann, R. J. (1977). Indices descriptive of factor complexity. \emph{The Journal of General Psychology}, 96(1), 103-110.
#'
#' @export
HofmannFac <- function(datos) {
  # If the data is a matrix without column names, assign default names automatically
  if (is.null(colnames(datos))) {
    colnames(datos) <- paste("F", 1:ncol(datos), sep = "")
  }
  
  # Step 1: Transpose the factor loading matrix
  datos_transpuestos <- t(datos)
  
  # Step 2: Square each factor loading in each row (now for each factor)
  datos_cuadrado <- datos_transpuestos^2
  
  # Step 3: Sum the squared factor loadings across all columns
  hoffman_num <- rowSums(datos_cuadrado)^2
  
  # Step 4: Raise each factor loading to the fourth power in each row
  datos_cuarta <- datos_transpuestos^4
  
  # Step 5: Sum the fourth power of the factor loadings across all columns
  hoffman_denom <- rowSums(datos_cuarta)
  
  # Step 6: Calculate Choff for the factors (factor complexity)
  Choff <- hoffman_num / hoffman_denom
  
  # Create a data frame with the results (only Choff)
  resultado <- data.frame(Choff = Choff)
  
  return(resultado)
}
