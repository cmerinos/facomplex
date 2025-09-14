#' @title Entropy Index for Factor Simplicity
#'
#' @description
#' Computes entropy-based indices to quantify the factorial simplicity or complexity of an Exploratory Factor Analysis (EFA) solution.
#' Entropy is computed at three levels: by item, by factor, and globally. Two types of entropy measures are available: normalized and scaled.
#'
#' @param loadings_matrix A numeric matrix or data frame of factor loadings, where rows represent items and columns represent factors.
#' @param base The logarithmic base used to compute entropy. Default is \code{2}, corresponding to entropy in bits.
#' @param normalized Logical. If \code{TRUE} (default), entropy values are normalized to range from 0 to 1 by dividing by \eqn{\log_b(k)} or \eqn{\log_b(n)}.
#' @param scaled Logical. If \code{TRUE}, returns the scaled entropy index and the theoretical minimum entropy as proposed by Beisel & Moreteau (1997). Default is \code{FALSE}.
#' @param bounded Logical. If \code{TRUE} (default), forces scaled entropy values to remain within the [0, 1] range by truncating negative or >1 values.
#' @param nd Integer. Number of decimal places to round the results. Default is \code{3}. Use \code{NULL} for no rounding.
#'
#' @details
#' The entropy index is based on the squared factor loadings (\eqn{\lambda_{ij}^2}), interpreted as the proportion of shared variance between item \eqn{i} and factor \eqn{j} (Shannon, 1948).
#'
#' \strong{1. Normalized Entropy:}
#' \itemize{
#'   \item For each item \eqn{i}, define the pseudo-proportions:
#'     \deqn{p_{ij} = \frac{\lambda_{ij}^2}{\sum_{j=1}^k \lambda_{ij}^2}}
#'   \item Then compute Shannon entropy:
#'     \deqn{H_i = - \sum_{j=1}^k p_{ij} \log_b(p_{ij})}
#'   \item If \code{normalized = TRUE}, divide by \eqn{\log_b(k)} to constrain values to [0, 1].
#' }
#'
#' The same logic applies for factors (across items), replacing \eqn{p_{ij}} with:
#' \deqn{q_{ij} = \frac{\lambda_{ij}^2}{\sum_{i=1}^n \lambda_{ij}^2}}
#'
#' \strong{2. Global Entropy:}
#' Entropy can also be calculated for the full loading matrix as a whole:
#' \deqn{p_{ij} = \frac{\lambda_{ij}^2}{\sum_{i,j} \lambda_{ij}^2}}
#' \deqn{H = - \sum_{i,j} p_{ij} \log_b(p_{ij})}
#' and normalized by \eqn{\log_b(n \cdot k)}.
#'
#' \strong{3. Scaled Entropy (Beisel & Moreteau, 1997):}
#' When \code{scaled = TRUE}, the function returns:
#' \itemize{
#'   \item \eqn{H_{min}}: A theoretical lower bound for entropy when one factor dominates:
#'     \deqn{H_{min} = - [p_{max} \log_b(p_{max}) + (1 - p_{max}) \log_b((1 - p_{max}) / (k - 1))]}
#'   \item \eqn{H_{scaled}}: A scaled measure between \eqn{H_{min}} and \eqn{H_{max}}:
#'     \deqn{H_{scaled} = \frac{H - H_{min}}{H_{max} - H_{min}}}
#' }
#'
#' \strong{4. Argument \code{bounded}:}
#' Scaled entropy can occasionally produce values outside [0, 1] if entropy is below the theoretical minimum.
#' If \code{bounded = TRUE}, the function truncates those values to stay within [0, 1] for interpretive clarity.
#'
#' @return A list with:
#' \describe{
#'   \item{\code{Hnormalized}}{A list with entropy by item, factor, and total.}
#'   \item{\code{Hscaled}}{A list with \code{Hmin.items}, \code{Hscaled.items}, and \code{Hscaled.total} (if \code{scaled = TRUE}).}
#' }
#'
#'#' \strong{Interpretation:}  
#' \itemize{
#'   \item Values near 0 indicate high factorial simplicity (loadings concentrated on a single factor).  
#'   \item Values near 1 suggest factorial complexity or ambiguity (dispersed loadings across factors).  
#'   \item \code{Hnormalized} expresses entropy as a proportion of the maximum possible entropy.  
#'   \item \code{Hscaled} expresses entropy relative to its theoretical minimum and maximum, allowing finer differentiation across contexts.  
#'   \item The index can be used to compare different rotation methods, number of factors, or item structures.
#' }
#'
#' Although no formal cutoff exists, entropy values below 0.20 typically reflect strong factorial simplicity,
#' while values near or above 0.80 may indicate multidimensionality or poor simple structure.
#' Interpretation should always be contextualized using additional indices, visual inspection of loadings, and substantive theory.
#' 
#' @examples
#' # Example: items with different factorial complexity
#' loadings <- matrix(c(
#'   0.7, 0.0, 0.01,
#'   0.1, 0.2, 0.15,
#'   0.4, 0.8, 0.2,
#'   0.4, 0.4, 0.4
#' ), nrow = 4, byrow = TRUE)
#'
#' entropyFL(loadings, normalized = TRUE, scaled = TRUE, bounded = TRUE)
#'
#' @references
#' Shannon, C. E. (1948). A mathematical theory of communication. \emph{Bell System Technical Journal}, 27(3), 379–423. \doi{10.1002/j.1538-7305.1948.tb00917.x}
#'
#' Beisel, J. N., & Moreteau, J.-C. (1997). A new method to estimate the lower bound of the Shannon-Wiener index of diversity. \emph{Ecological Modelling}, 99(1), 99–105.
#'
#' Hofmann, R. J. (1978). Complexity and simplicity as objective indices descriptive of factor solutions. \emph{Multivariate Behavioral Research}, 13(2), 247–250.
#'
#' Lorenzo-Seva, U. (2003). A factor simplicity index. \emph{Psychometrika}, 68(1), 49–60. \doi{10.1007/BF02296652}
#' 
#' @export
entropyFL <- function(loadings_matrix, base = 2, normalized = TRUE, scaled = FALSE, bounded = TRUE, nd = 3) {
  if (!(is.matrix(loadings_matrix) || is.data.frame(loadings_matrix))) {
    stop("Input must be a matrix or data.frame")
  }
  loadings_matrix <- as.matrix(loadings_matrix)
  n_items <- nrow(loadings_matrix)
  n_factors <- ncol(loadings_matrix)
  load_sq <- loadings_matrix^2
  
  # --- H_i: Entropía por ítem
  row_sums <- rowSums(load_sq)
  pij <- sweep(load_sq, 1, row_sums, FUN = "/")
  pij[is.nan(pij)] <- 0
  H_i <- -rowSums(ifelse(pij > 0, pij * log(pij, base = base), 0))
  Hmax_i <- log(n_factors, base = base)
  if (normalized) H_i <- H_i / Hmax_i
  
  # --- H_f: Entropía por factor
  col_sums <- colSums(load_sq)
  qij <- sweep(load_sq, 2, col_sums, FUN = "/")
  qij[is.nan(qij)] <- 0
  H_f <- -colSums(ifelse(qij > 0, qij * log(qij, base = base), 0))
  if (normalized) H_f <- H_f / log(n_items, base = base)
  
  # --- H_total (matriz completa)
  p_all <- load_sq / sum(load_sq)
  p_all[is.nan(p_all)] <- 0
  H_total <- -sum(ifelse(p_all > 0, p_all * log(p_all, base = base), 0))
  if (normalized) H_total <- H_total / log(n_items * n_factors, base = base)
  
  # --- Resultados normalizados
  result <- list(
    Hnormalized = list(
      H.items  = if (!is.null(nd)) round(H_i, nd) else H_i,
      H.factors = if (!is.null(nd)) round(H_f, nd) else H_f,
      H.total   = if (!is.null(nd)) round(H_total, nd) else H_total
    )
  )
  
  # --- Resultados escalados (Beisel)
  if (scaled) {
    max_p <- apply(pij, 1, max)
    Hmin_i <- - (max_p * log(max_p, base = base) + (1 - max_p) * log((1 - max_p)/(n_factors - 1), base = base))
    Hmin_i[is.nan(Hmin_i)] <- 0
    if (normalized) Hmin_i <- Hmin_i / Hmax_i
    Hscaled_i <- (H_i - Hmin_i) / (Hmax_i - Hmin_i)
    
    if (bounded) {
      Hscaled_i <- pmin(1, pmax(0, Hscaled_i))
    }
    
    Hscaled_total <- H_total  # matemáticamente equivalente
    
    result$Hscaled <- list(
      Hmin.items     = if (!is.null(nd)) round(Hmin_i, nd) else Hmin_i,
      Hscaled.items  = if (!is.null(nd)) round(Hscaled_i, nd) else Hscaled_i,
      Hscaled.total  = if (!is.null(nd)) round(Hscaled_total, nd) else Hscaled_total
    )
  }
  
  return(result)
}
