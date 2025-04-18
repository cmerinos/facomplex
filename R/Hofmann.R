#' Hofmann: Hofmann's Coefficient for assessing the factorial complexity of items
#'
#' This function calculates the Hofmann coefficient (\code{Hofmann}, Hofmann, 1977) to
#'  evaluate the factorial simplicity of items in a factorial model.
#' It also implements a modified version of \code{Hofmann}, called \code{Hoff_R}, 
#' with values normalized between 0 and 1. The \code{Hofmann} coefficient ranges from 1 
#' (indicating factorial simplicity or unidimensionality) to p (the total number 
#' of factors in the model, indicating greater complexity).
#'
#' @param data A \code{data.frame} containing the factor loadings. Rows correspond 
#' to items, and columns correspond to factors in the factorial model.
#' The values should be factor loadings, typically ranging from -0.9999 to 0.9999.
#'
#' @return A \code{data.frame} with two numeric vectors:
#' \itemize{
#'   \item \code{Hoff}: The Hofman coefficient calculated for each item, 
#'   indicating factorial complexity.
#'   \item \code{Hoff_R}: The modified Hofman coefficient, where
#'    \code{Hoff_R = 1 / Choff}, with values normalized between 0 and 1.
#' }
#'
#' @details
#' Hofmann (1977) proposed the factorial complexity coefficient (\code{Hofmann}), 
#' which ranges from 1 (indicating that the item has a unidimensional structure,
#' i.e., factorial simplicity) to p (the total number of factors in the model, 
#' indicating that the item has a more complex structure). 
#' This coefficient is calculated from the factor loadings of the items, assessing
#'  how much variance each factor explains for each item. 
#' 
#' In this case, the modification of \code{Hofmann} (denoted \code{Hoff_R}) is
#'  obtained as the inverse of \code{Hofmann}, resulting in values between 0 and 1.
#'  A value close to 1 for \code{Hoff_R} indicates greater factorial simplicity.
#'
#' The values of \code{Hoff} and \code{Hoff_R} should be interpreted in the 
#' context of the factorial model and the user's judgment, as a single value may
#' not be sufficient for a precise interpretation of the factorial simplicity or 
#' unidimensionality of the items.
#'
#' @examples
#' # Create a data frame with example factor loadings
#' ex1.data <- data.frame(
#'   F1 = c(0.536, 0.708, 0.600, 0.673, 0.767, 0.481, -0.177, 0.209, -0.097, -0.115, 0.047, 0.024),
#'   F2 = c(-0.11, 0.026, 0.076, 0.011, -0.16, 0.106, 0.668, 0.438, 0.809, 0.167, 0.128, 0.041),
#'   F3 = c(-0.1, 0.036, 0.086, 0.021, -0.15, 0.116, 0.678, 0.448, 0.819, 0.577, 0.738, 0.751)
#' )
#' 
#' # Call the function to calculate the Hofman coefficient
#' results <- Hofmann(ex1.data)
#' 
#' # View the results
#' print(results)
#'
#' @references
#' Hofmann, J. (1977). A coefficient of complexity for factorial structures.
#'  \emph{Psychometrika}, 42(4), 453-464. 
#'
#' @export
Hofmann <- function(data) {
  # Paso 2: Elevar al cuadrado cada carga factorial en cada fila
  datos_cuadrado <- data^2
  
  # Paso 3: Sumar las cargas factoriales elevadas al cuadrado de todas las columnas
  hoffman_num <- rowSums(datos_cuadrado)^2
  
  # Paso 5: Elevar a la cuarta potencia cada carga factorial en cada fila
  datos_cuarta <- data^4
  
  # Paso 6: Sumar las cargas factoriales elevadas a la cuarta potencia de todas las columnas
  hoffman_denom <- rowSums(datos_cuarta)
  
  # Paso 7: Calcular Choff
  Choff <- hoffman_num / hoffman_denom
  
  # Paso 8: Modificar Choff como 1/Choff para obtener Choff_R
  Choff_R <- 1 / Choff
  
  # Crear el data frame con los resultados
  resultado <- data.frame(Hoff = Choff, Hoff_R = Choff_R)
  
  return(round(resultado, 3))
  
  return(resultado)
}
