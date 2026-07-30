#' Hofmann Index of Factorial Complexity and Scaled Versions
#'
#' Computes Hofmann's (1977) coefficient of factorial complexity for each item 
#' in a factor loading matrix, along with optionally scaled versions for 
#' complexity (0–1) and simplicity (0–1).
#'
#' The original Hofmann index (\code{CHof}) quantifies the extent to which an item 
#' loads on multiple factors. It ranges from 1 (perfect factorial simplicity, 
#' i.e., loading only on one factor) to \eqn{p} (maximum complexity, where the 
#' item loads equally on all \eqn{p} factors). 
#' 
#' The scaled complexity version (\code{CHof.R}, when \code{scaledVersion = TRUE}) 
#' linearly transforms the original index to a 0–1 scale:
#' \deqn{CHof.R = \frac{CHof - 1}{p - 1}}
#' where \eqn{p} is the number of factors. Values close to 0 indicate high 
#' factorial simplicity; values close to 1 indicate high complexity.
#'
#' The simplicity version (\code{CHof.S}, when \code{simplVersion = TRUE}) is 
#' the complement of the scaled complexity:
#' \deqn{CHof.S = \frac{p - CHof}{p - 1} = 1 - CHof.R}
#' Values close to 1 indicate high factorial simplicity; values close to 0 
#' indicate high complexity.
#'
#' @param data A numeric data frame or matrix of factor loadings, where rows 
#'             represent items and columns represent factors. Factor loadings 
#'             are typically between -1 and 1.
#' @param scaledVersion Logical; if \code{TRUE}, adds a column \code{CHof.R} 
#'        with complexity scaled to 0 to 1. Default is \code{FALSE}.
#' @param simplVersion Logical; if \code{TRUE}, adds a column \code{CHof.S} 
#'        with simplicity scaled to 0 to 1. Default is \code{FALSE}.
#'
#' @return A data frame with the original \code{CHof} column and optionally 
#'         \code{CHof.R} (scaled complexity) and/or \code{CHof.S} (scaled 
#'         simplicity) depending on the arguments.
#'
#' @details
#' The Hofmann complexity index for item \eqn{i} is computed as:
#' \deqn{CHof_i = \frac{(\sum_{j=1}^{p} \lambda_{ij}^2)^2}{\sum_{j=1}^{p} \lambda_{ij}^4}}
#' where \eqn{\lambda_{ij}} is the loading of item \eqn{i} on factor \eqn{j}, 
#' and \eqn{p} is the number of factors.
#'
#' The scaled versions are useful for comparing items across studies with 
#' different numbers of factors, as they are bounded between 0 and 1. 
#' They complement other simplicity indices such as Bentler's or Fleming's.
#'
#' @references
#' Hofmann, R. J. (1977). Indices descriptive of factor complexity. 
#' \emph{The Journal of General Psychology}, 96(1), 103-110. 
#' \doi{10.1080/00221309.1977.9920803}
#' 
#' Pettersson, E., & Turkheimer, E. (2014). Self-Reported Personality Pathology 
#' Has Complex Structure and Imposing Simple Structure Degrades Test Information. 
#' \emph{Multivariate Behavioral Research}, 49(4), 372-389. 
#' \doi{10.1080/00273171.2014.911073}
#'
#' Pettersson, E., & Turkheimer, E. (2010). Item selection, evaluation, and 
#' simple structure in personality data. \emph{Journal of Research in Personality}, 
#' 44(4), 407-420. \doi{10.1016/j.jrp.2010.03.002}
#'
#' @examples
#' # Simulated factor loadings (3 factors)
#' loadings <- data.frame(
#'   F1 = c(0.536, 0.708, 0.600, 0.673, 0.767, 0.481, -0.177, 0.209, -0.097, -0.115, 0.047, 0.024),
#'   F2 = c(-0.11, 0.026, 0.076, 0.011, -0.16, 0.106, 0.668, 0.438, 0.809, 0.167, 0.128, 0.041),
#'   F3 = c(-0.1, 0.036, 0.086, 0.021, -0.15, 0.116, 0.678, 0.448, 0.819, 0.577, 0.738, 0.751)
#' )
#'
#' # Original Hofmann index only
#' Hofmann(loadings)
#'
#' # With scaled complexity (0-1)
#' Hofmann(loadings, scaledVersion = TRUE)
#'
#' # With simplicity (0-1)
#' Hofmann(loadings, simplVersion = TRUE)
#'
#' # Both scaled versions
#' Hofmann(loadings, scaledVersion = TRUE, simplVersion = TRUE)
#'
#' @export

Hofmann <- function(data, scaledVersion = FALSE, simplVersion = FALSE) {
  
  # Convert matrices to data frame
  if (is.matrix(data)) {
    data <- as.data.frame(data)
  }
  
  # Verify that data is a data frame
  if (!is.data.frame(data)) {
    stop("The argument 'data' must be a data frame or a matrix")
  }
  
  # Number of factors (columns)
  p <- ncol(data)
  
  # Compute squared and fourth-power loadings
  datos_cuadrado <- data^2
  datos_cuarta <- data^4
  
  # Numerator and denominator for Hofmann index
  hofmann_num <- rowSums(datos_cuadrado)^2
  hofmann_denom <- rowSums(datos_cuarta)
  
  # Original Hofmann complexity (range: 1 to p)
  Chof <- hofmann_num / hofmann_denom
  
  # Start result data frame with CHof
  resultado <- data.frame(CHof = Chof)
  
  # Scaled complexity (0-1) if requested
  if (scaledVersion) {
    if (p == 1) {
      Chof_R <- rep(0, length(Chof))
    } else {
      Chof_R <- (Chof - 1) / (p - 1)
    }
    resultado$CHof.R <- Chof_R
  }
  
  # Simplicity (0-1) if requested
  if (simplVersion) {
    if (p == 1) {
      Chof_S <- rep(1, length(Chof))
    } else {
      Chof_S <- (p - Chof) / (p - 1)
    }
    resultado$CHof.S <- Chof_S
  }
  
  # Round all numeric columns to 3 decimals for cleaner output
  resultado <- round(resultado, 3)
  
  return(resultado)
}