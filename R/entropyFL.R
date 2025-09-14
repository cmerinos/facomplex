#' @title Entropy Index for Factor Simplicity
#'
#' @description
#' Computes entropy-based indices to quantify the factorial simplicity or complexity of an Exploratory Factor Analysis (EFA) solution.
#' The entropy is calculated from the squared factor loadings, interpreted as proportional contributions of each factor to an item (or vice versa).
#' Entropy is computed at three levels: by item, by factor, and globally.
#'
#' @param loadings_matrix A numeric matrix or data frame of factor loadings, where rows represent items and columns represent factors.
#' @param base The logarithmic base used to compute entropy. Default is \code{2}, corresponding to entropy in bits.
#' @param normalized Logical. If \code{TRUE} (default), entropy values are normalized to range from 0 to 1.
#' @param scaled Logical. If \code{TRUE}, also returns the scaled entropy index and the theoretical minimum entropy (\eqn{H_{min}}). Default is \code{FALSE}.
#' @param nd Integer. Number of decimal places to round the results. Default is \code{3}. Use \code{NULL} for no rounding.
#'
#' @details
#' The function assumes that the squared factor loadings (\eqn{\lambda_{ij}^2}) represent the proportion of common variance
#' that item \eqn{i} shares with factor \eqn{j}. These are normalized within rows or columns to form pseudo-probability distributions,
#' over which Shannon entropy is computed.
#'
#' \strong{1. Entropy by item:}  
#' For each item \eqn{i}, let \eqn{\lambda_{ij}} be its loading on factor \eqn{j}, with \eqn{j = 1, ..., k}. Define:
#'
#' \deqn{p_{ij} = \frac{\lambda_{ij}^2}{\sum_{j=1}^{k} \lambda_{ij}^2}}
#'
#' Then the entropy for item \eqn{i} is:
#'
#' \deqn{H_i = - \sum_{j=1}^{k} p_{ij} \log_b(p_{ij})}
#'
#' If \code{normalized = TRUE}, the value is divided by \eqn{\log_b(k)} to constrain the index to [0, 1].
#' Lower values indicate that most of the variance is concentrated in one factor (simple structure); higher values indicate cross-loadings.
#'
#' \strong{2. Entropy by factor:}  
#' Similarly, for each factor \eqn{j}, entropy is calculated over its squared loadings across items:
#'
#' \deqn{q_{ij} = \frac{\lambda_{ij}^2}{\sum_{i=1}^{n} \lambda_{ij}^2}}
#'
#' \deqn{H_j = - \sum_{i=1}^{n} q_{ij} \log_b(q_{ij})}
#'
#' Normalization is done by dividing by \eqn{\log_b(n)}.
#' A factor with low entropy loads strongly on only a few items; high entropy suggests broad dispersion across many items.
#'
#' \strong{3. Total entropy:}  
#' The total entropy can be interpreted as a global index of factorial complexity:
#' \itemize{
#'   \item \code{Htotal.items}: Average normalized entropy across items.
#'   \item \code{Htotal.factors}: Average normalized entropy across factors.
#' }
#'
#' \strong{4. Minimum and scaled entropy (optional):}  
#' When \code{scaled = TRUE}, the function returns additional values:
#' \itemize{
#'   \item \code{Hmin.items}: Theoretical minimum entropy for each item, based on Beisel & Moreteau (1997)
#'   \item \code{Hscaled.items}: Scaled entropy index for each item:
#'   \deqn{H_{scaled} = \frac{H - H_{min}}{H_{max} - H_{min}}}
#'   where \eqn{H_{max} = \log_b(k)}
#' }
#' This scaled index is useful for comparing factorial complexity across studies with different numbers of factors.
#'
#' \strong{Interpretation:}  
#' \itemize{
#'   \item Values near 0 indicate highly simple structures (loadings concentrated in few components).  
#'   \item Values near 1 suggest factorial ambiguity or complexity.  
#'   \item \code{Hnormalized} compares \eqn{H} to the maximum entropy.  
#'   \item \code{Hscaled} compares \eqn{H} relative to its minimum and maximum, offering finer distinctions.  
#'   \item Useful to compare different rotation methods, number of factors, or loading patterns.
#' }
#'
#' Although no formal cutoff exists, lower entropy values (e.g., < 0.20) typically reflect strong
#' factorial simplicity, whereas values approaching 1 indicate dispersed or ambiguous loading 
#' patterns. Interpretation should be contextualized with additional indices and visual inspection.
#'
#' @return A list with:
#' \describe{
#'   \item{\code{Hitems}}{A numeric vector with the normalized entropy for each item.}
#'   \item{\code{Hfactors}}{A numeric vector with the normalized entropy for each factor.}
#'   \item{\code{Htotal.items}}{The average normalized entropy across items.}
#'   \item{\code{Htotal.factors}}{The average normalized entropy across factors.}
#'   \item{\code{Hmin.items}}{Theoretical minimum entropy for each item (if \code{scaled = TRUE}).}
#'   \item{\code{Hscaled.items}}{Scaled entropy index for each item (if \code{scaled = TRUE}).}
#' }
#'
#' @examples
#' # Example: items with different factorial complexity
#' loadings <- matrix(c(
#'   0.7, 0.0, 0.01,  # simple item
#'   0.1, 0.2, 0.15,  # moderately complex
#'   0.4, 0.8, 0.2,   # complex with a dominant factor
#'   0.4, 0.4, 0.4    # maximally complex (equal loadings)
#' ), nrow = 4, byrow = TRUE)
#' 
#' entropyFL(loadings, scaled = TRUE)
#'
#' @references
#' Shannon, C. E. (1948). A mathematical theory of communication. \emph{Bell System Technical Journal}, 27(3), 379--423. \doi{10.1002/j.1538-7305.1948.tb00917.x}  
#' Hofmann, R. J. (1978). Complexity and simplicity as objective indices descriptive of factor solutions. \emph{Multivariate Behavioral Research}, 13(2), 247--250.  
#' Lorenzo-Seva, U. (2003). A factor simplicity index. \emph{Psychometrika}, 68(1), 49--60. \doi{10.1007/BF02296652}  
#' Beisel, J. N., & Moreteau, J.-C. (1997). A new method to estimate the lower bound of the Shannon-Wiener index of diversity. \emph{Ecological Modelling}, 99(1), 99--105.
#'
#' @export
entropyFL <- function(loadings_matrix, base = 2, normalized = TRUE, scaled = FALSE, nd = 3) {
  if (!(is.matrix(loadings_matrix) || is.data.frame(loadings_matrix))) {
    stop("Input must be a matrix or data.frame")
  }
  loadings_matrix <- as.matrix(loadings_matrix)
  n_items <- nrow(loadings_matrix)
  n_factors <- ncol(loadings_matrix)
  
  load_sq <- loadings_matrix^2
  
  row_sums <- rowSums(load_sq)
  pij <- sweep(load_sq, 1, row_sums, FUN = "/")
  pij[is.nan(pij)] <- 0
  
  H_i <- -rowSums(ifelse(pij > 0, pij * log(pij, base = base), 0))
  Hmax <- log(n_factors, base = base)
  if (normalized) H_i <- H_i / Hmax
  
  col_sums <- colSums(load_sq)
  qij <- sweep(load_sq, 2, col_sums, FUN = "/")
  qij[is.nan(qij)] <- 0
  
  H_f <- -colSums(ifelse(qij > 0, qij * log(qij, base = base), 0))
  if (normalized) H_f <- H_f / log(n_items, base = base)
  
  H_total_items <- mean(H_i)
  H_total_factors <- mean(H_f)
  
  result <- list(
    Hitems = if (!is.null(nd)) round(H_i, nd) else H_i,
    Hfactors = if (!is.null(nd)) round(H_f, nd) else H_f,
    Htotal.items = if (!is.null(nd)) round(H_total_items, nd) else H_total_items,
    Htotal.factors = if (!is.null(nd)) round(H_total_factors, nd) else H_total_factors
  )
  
  if (scaled) {
    max_p <- apply(pij, 1, max)
    Hmin_i <- - (max_p * log(max_p, base = base) + (1 - max_p) * log((1 - max_p) / (n_factors - 1), base = base))
    Hmin_i[is.nan(Hmin_i)] <- 0
    if (normalized) Hmin_i <- Hmin_i / Hmax
    Hscaled_i <- (H_i - Hmin_i) / (Hmax - Hmin_i)
    
    result$Hmin.items <- if (!is.null(nd)) round(Hmin_i, nd) else Hmin_i
    result$Hscaled.items <- if (!is.null(nd)) round(Hscaled_i, nd) else Hscaled_i
  }
  
  return(result)
}
