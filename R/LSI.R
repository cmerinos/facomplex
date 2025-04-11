#' @title LSI - Loading Simplicity Index (Lorenzo-Seva, 2003)
#' @description Computes the Loading Simplicity Index (LSI) proposed by Lorenzo-Seva (2003), both globally and item-wise, based on a matrix of factor loadings.
#'
#' @param loadings A matrix or data frame of factor loadings. Rows represent items and columns represent factors.
#'
#' @return A list with two components:
#' \itemize{
#'   \item \code{LSI.item}: A numeric vector with the LSI value computed for each item.
#'   \item \code{LSI.global}: A single numeric value representing the overall simplicity index.
#' }
#'
#' @details
#' The Loading Simplicity Index (LSI) quantifies how clearly each item loads on a single factor. The index is calculated using a nonlinear weighting of squared loadings:
#'
#' \deqn{LSI_i = \frac{1}{r} \sum_{j=1}^r (l_{ij}^2 + \epsilon) \cdot 10^{l_{ij}^2}}
#'
#' where \eqn{l_{ij}} is the loading of item \eqn{i} on factor \eqn{j}, and \eqn{r} is the number of factors.
#' 
#' The global LSI is calculated as:
#' \deqn{LS = \frac{w - e}{1 - e}}
#' where \eqn{w} is the average of all \eqn{LSI_i}, and \eqn{e} is the theoretical minimum possible value of \eqn{w}, used for normalization.
#'
#' LSI values range from 0 (high complexity) to 1 (high simplicity).
#'
#' @references
#' Lorenzo-Seva, U. (2003). A factor simplicity index. *Psychometrika, 68*(1), 49–60.
#' https://doi.org/10.1007/BF02296652
#'
#' @seealso \code{\link{BSI}}, \code{\link{plot_simplicity}} for visualization.
#'
#' @examples
#' # Sample data
#' ex1_data <- data.frame(
#'   F1 = c(0.536, 0.708, 0.600, 0.673, 0.767, 0.481, -0.177, 0.209, -0.097, -0.115, 0.047, 0.024),
#'   F2 = c(-0.110, 0.026, 0.076, 0.011, -0.160, 0.106, 0.668, 0.438, 0.809, 0.167, 0.128, 0.041),
#'   F3 = c(-0.100, 0.036, 0.086, 0.021, -0.150, 0.116, 0.678, 0.448, 0.819, 0.577, 0.738, 0.751)
#' )
#'
#' # Compute the Loading Simplicity Index
#' LSI(ex1_data)
#'
#' # View item-level and global results
#' LSI(ex1_data)$LSI.item)
#' LSI(ex1_data)$LSI.global
#'
#' @author Cesar Merino-Sotro
#' @export
LSI <- function(loadings) {
  # Validar que las cargas factoriales sean una matriz o data.frame
  if (!is.matrix(loadings) && !is.data.frame(loadings)) {
    stop("El argumento 'loadings' debe ser una matriz o un data.frame.")
  }
  
  # Convertir a matriz si es necesario
  B <- as.matrix(loadings)
  
  # Dimensiones
  p <- nrow(B) # Número de variables
  r <- ncol(B) # Número de factores
  
  # Constante epsilon
  epsilon <- 1e-6
  
  # Calcular w por ítem
  w_items <- apply(B, 1, function(row) {
    sum((row^2 + epsilon) * 10^(row^2)) / r
  })
  
  # Promedio de w para todos los ítems (índice global w)
  w_global <- mean(w_items)
  
  # Calcular e (mínimo posible de w)
  e <- (1 / r) * sum((1 / (1 + epsilon)) * 10^epsilon)
  
  # Calcular LS (Loading Simplicity Index) global
  LS_global <- (w_global - e) / (1 - e)
  
  # Calcular LS para cada ítem
  LS_items <- (w_items - e) / (1 - e)
  
  # Retornar resultados como lista
  return(list(
    LSI.item = LS_items,   # Índice por ítem
    LSI.global = LS_global     # Índice global
  ))
}
