#' Hofmann Index of Factorial Complexity and its Normalized Inverse
#'
#' Computes Hofmann’s (1977) coefficient of factorial complexity for each item 
#' in a factor loading matrix, along with a normalized inverse version rescaled between 0 and 1.
#'
#' The original Hofmann index (\code{Hofmann}) quantifies the extent to which an item loads 
#' on multiple factors. It ranges from 1 (perfect factorial simplicity, i.e., loading only 
#' on one factor) to \eqn{p} (maximum complexity, where the item loads equally on all \eqn{p} factors).
#' 
#' The modified version is the reciprocal of \code{Hofmann}, resulting in a simplicity index
#' normalized to the interval [0, 1]. Higher values indicate greater factorial simplicity.
#'
#' @param data A numeric data frame or matrix of factor loadings, where rows represent items 
#' and columns represent factors. Factor loadings are typically between -1 and 1.
#'
#' @return A data frame with two columns:
#' \itemize{
#'   \item \code{CHof}: Hofmann's original complexity coefficient for each item.
#'   \item \code{CHof_R}: The inverse of \code{Hoff}, representing factorial simplicity in [0, 1].
#' }
#'
#' @details
#' The Hofmann complexity index is computed as:
#' \deqn{Hoff_i = \frac{(\sum_j \lambda_{ij}^2)^2}{\sum_j \lambda_{ij}^4}}
#' where \eqn{\lambda_{ij}} is the loading of item \eqn{i} on factor \eqn{j}.
#'
#' The rescaled index \code{Hoff_R} is computed as \code{1 / Hoff}, and provides a 
#' bounded indicator of simplicity (closer to 1 means simpler structure).
#' 
#' These indices are particularly useful when comparing items in terms of their factorial clarity, 
#' and complement other measures such as Bentler’s or Fleming’s simplicity indices. An extensive use of this 
#' coefficient can be found in: Pettersson & Turkheimer (2010, 2014).
#'
#' @examples
#' # Simulated factor loadings
#' ex1.data <- data.frame(
#'   F1 = c(0.536, 0.708, 0.600, 0.673, 0.767, 0.481, -0.177, 0.209, -0.097, -0.115, 0.047, 0.024),
#'   F2 = c(-0.11, 0.026, 0.076, 0.011, -0.16, 0.106, 0.668, 0.438, 0.809, 0.167, 0.128, 0.041),
#'   F3 = c(-0.1, 0.036, 0.086, 0.021, -0.15, 0.116, 0.678, 0.448, 0.819, 0.577, 0.738, 0.751)
#' )
#' Hofmann(ex1.data)
#'
#' @references
#' Hofmann, R. J. (1977). Indices descriptive of factor complexity. \emph{The Journal of General Psychology}, 96(1), 103–110. 
#' https://doi.org/10.1080/00221309.1977.9920803
#' 
#' Pettersson, E., & Turkheimer, E. (2014). Self-Reported Personality Pathology Has Complex Structure and Imposing Simple 
#' Structure Degrades Test Information. \emph{Multivariate Behavioral Research}, 49(4), 372–389. https://doi.org/10.1080/00273171.2014.911073
#'
#' Pettersson, E., & Turkheimer, E. (2010). Item selection, evaluation, and simple structure in personality data. \emph{Journal 
#' of Research in Personality}, 44(4), 407–420. https://doi.org/10.1016/j.jrp.2010.03.002 
#'
#' @export
Hofmann <- function(data) {
  # Paso 2: Elevar al cuadrado cada carga factorial en cada fila
  datos_cuadrado <- data^2
  
  # Paso 3: Sumar las cargas factoriales elevadas al cuadrado de todas las columnas
  hofmann_num <- rowSums(datos_cuadrado)^2
  
  # Paso 5: Elevar a la cuarta potencia cada carga factorial en cada fila
  datos_cuarta <- data^4
  
  # Paso 6: Sumar las cargas factoriales elevadas a la cuarta potencia de todas las columnas
  hofmann_denom <- rowSums(datos_cuarta)
  
  # Paso 7: Calcular Choff
  Chof <- hofmann_num / hofmann_denom
  
  # Paso 8: Modificar Choff como 1/Choff para obtener Chof_R
  Chof_R <- 1 / Chof
  
  # Crear el data frame con los resultados
  resultado <- data.frame(CHof = Chof, CHof_R = Chof_R)
  
  return(round(resultado, 3))
  
  return(resultado)
}
