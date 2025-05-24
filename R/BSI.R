#' @title BSI - Bentler Simplicity Index
#'
#' @description 
#' Computes the Bentler Simplicity Index (BSI), a scale-free matrix measure that quantifies the factorial simplicity of a solution. 
#' It is derived from the squared factor loadings and indicates the extent to which the factor structure approaches a perfectly simple pattern.
#'
#' @param data A matrix or data frame of factor loadings with \code{k} rows (items) and \code{n} columns (factors).
#' @param item_names Optional vector of item names. If \code{NULL}, default item labels will be generated.
#' @param sort_items Determines the ordering of items in the output. Options are \code{"up"} (ascending), \code{"down"} (descending), or \code{NULL} (retain original order). Default is \code{NULL}.
#'
#' @return A list with two components:
#' \itemize{
#'   \item \code{BSI_per_item}: A data frame with the Bentler simplicity value computed for each item.
#'   \item \code{BSI_global}: A scalar value representing the global Bentler Simplicity Index (i.e., the mean across all items).
#' }
#'
#' @details
#' The BSI quantifies the simplicity of the factorial structure at the item level by assessing the 
#' degree to which each item predominantly loads on a single factor. It is computed using the formula:
#'
#' \deqn{BSI_i = \frac{\sum_j a_{ij}^4}{\left( \sum_j a_{ij}^2 \right)^2}}
#'
#' where \eqn{a_{ij}} is the loading of item \eqn{i} on factor \eqn{j}. This adaptation follows the logic 
#' of the global Bentler Simplicity Index, but applies it row-wise (per item) instead of matrix-wise. 
#' While Bentler (1977) originally defined the BSI as a scale-level index, computing it at the item level 
#' offers a complementary view for identifying items that may contribute disproportionately to factorial complexity.
#'
#' The global BSI reported by this function is simply the mean of the item-level BSI values.
#'
#' \strong{Interpretation:} In practice, a BSI value near 1.0 suggests that the item reflects a "pure" or
#' unidimensional measure aligned with a single latent factor. Conversely, lower values (e.g., below 0.50) may 
#' indicate factorial complexity, potentially undermining the interpretability or construct validity of 
#' that item within a scale. This makes BSI especially useful for refining multidimensional scales by 
#' identifying items with cross-loadings.
#'
#' @references
#' Bentler, P. M. (1977). Factor simplicity index and transformations. \emph{Psychometrika, 42}(2), 
#' 277–295. https://doi.org/10.1007/BF02294054
#'
#' @seealso \code{\link{plot_simplicity}} for a graphical representation of BSI values.
#'
#' @examples
#' # Example data
#' ex1_data <- data.frame(
#'   F1 = c(0.536, 0.708, 0.600, 0.673, 0.767, 0.481, -0.177, 0.209, -0.097, -0.115, 0.047, 0.024),
#'   F2 = c(-0.110, 0.026, 0.076, 0.011, -0.160, 0.106, 0.668, 0.438, 0.809, 0.167, 0.128, 0.041),
#'   F3 = c(-0.100, 0.036, 0.086, 0.021, -0.150, 0.116, 0.678, 0.448, 0.819, 0.577, 0.738, 0.751)
#' )
#'
#' # Compute BSI
#' BSI_result <- BSI(ex1_data)
#'
#' # Sort by ascending order
#' BSI_result_up <- BSI(ex1_data, sort_items = "up")
#'
#' # Sort by descending order
#' BSI_result_down <- BSI(ex1_data, sort_items = "down")
#'
#' @author Cesar Merino-Soto
#' @export
BSI <- function(data, item_names = NULL, sort_items = NULL) {
  # Validar si el argumento es una matriz o data.frame
  if (!is.matrix(data) && !is.data.frame(data)) {
    stop("El argumento 'data' debe ser una matriz o un data.frame.")
  }
  
  # Convertir data.frame a matriz si es necesario
  loadings <- as.matrix(data)
  
  # Asegurarse de que los valores sean numéricos
  if (!is.numeric(loadings)) {
    stop("Los valores de 'data' deben ser numéricos (cargas factoriales).")
  }
  
  # Manejar posibles NA en la matriz
  if (any(is.na(loadings))) {
    warning("Se encontraron valores NA. Se ignorarán en los cálculos.")
    loadings[is.na(loadings)] <- 0  # Opcional: reemplazar NA con 0
  }
  
  # Calcular BSI por ítem (fila)
  bsi_values <- rowSums(loadings^4) / (rowSums(loadings^2)^2)
  
  # Calcular el índice de simplicidad global como promedio de los valores por ítem
  simplicity_index <- mean(bsi_values, na.rm = TRUE)
  
  # Si el usuario no proporciona nombres de ítems, generar nombres automáticos
  if (is.null(item_names)) {
    item_names <- paste0("Item.", seq_along(bsi_values))
  }
  
  # Crear un data frame con los valores de BSI
  results <- data.frame(
    Item = item_names,
    BSI.Value = round(bsi_values, 3),
    stringsAsFactors = FALSE
  )
  
  # Aplicar ordenamiento si el usuario lo indica
  if (!is.null(sort_items)) {
    if (sort_items == "up") {
      results <- results[order(results$BSI.Value), ]
    } else if (sort_items == "down") {
      results <- results[order(results$BSI.Value, decreasing = TRUE), ]
    } else {
      stop("El argumento 'sort_items' debe ser 'up', 'down' o NULL.")
    }
  }
  
  # Retornar los resultados
  return(list(
    BSI.item = results,
    BSI.global = round(simplicity_index, 3)
  ))
}
