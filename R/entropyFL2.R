#' @title Entropy Index for Factor Simplicity (Exploratory and Confirmatory)
#'
#' @description
#' Computes entropy-based indices to quantify the factorial simplicity or complexity 
#' of an Exploratory Factor Analysis (EFA) or confirmatory solution (e.g., target rotations, ESEM).
#' The entropy is calculated from the squared factor loadings, interpreted as proportional 
#' contributions of each factor to an item (or vice versa).
#' 
#' When the input matrix corresponds to **network loadings (NT)** obtained from 
#' Exploratory Graph Analysis (EGA), this index serves as a measure of 
#' **node-community assignment clarity**. Low entropy indicates that nodes load 
#' predominantly on a single community (simple structure), whereas high entropy 
#' suggests diffuse or ambiguous community membership, complementing the global 
#' fit assessment provided by the Total Entropy Fit Index (TEFI).
#'
#' @param loadings_matrix A numeric matrix or data frame of factor loadings, where rows represent items and columns represent factors.
#' @param base The logarithmic base used to compute entropy. Default is \code{2}, corresponding to entropy in bits.
#' @param normalized Logical. If \code{TRUE} (default), entropy values are normalized to range from 0 to 1.
#' @param nd Integer. Number of decimal places to round the results. Default is \code{3}. Use \code{NULL} for no rounding.
#' @param type Character string indicating the mode: \code{"expl"} for exploratory (entropy over all factors/items) 
#'        or \code{"conf"} for confirmatory (entropy collapses into target vs non-target categories). 
#'        Default is \code{"expl"}.
#' @param target A numeric vector of length \code{nrow(loadings_matrix)} indicating, for each item, 
#'        the factor index (1 to ncol(loadings_matrix)) to which it is expected to belong. 
#'        Required when \code{type = "conf"}.
#'
#' @details
#' The function assumes that the squared factor loadings (\eqn{\lambda_{ij}^2}) represent the proportion of common variance
#' that item \eqn{i} shares with factor \eqn{j}. These are normalized within rows or columns to form pseudo-probability distributions,
#' over which Shannon entropy is computed.
#'
#' \strong{Exploratory mode (\code{type = "expl"}):}
#' \itemize{
#'   \item Entropy by item: \eqn{H_i = -\sum_{j=1}^{k} p_{ij} \log(p_{ij})}, where \eqn{p_{ij} = \lambda_{ij}^2 / \sum_j \lambda_{ij}^2}.
#'   \item Entropy by factor: \eqn{H_f = -\sum_{i=1}^{n} q_{ij} \log(q_{ij})}, where \eqn{q_{ij} = \lambda_{ij}^2 / \sum_i \lambda_{ij}^2}.
#'   \item Normalization: divide by \eqn{\log(k)} for items, and by \eqn{\log(n)} for factors.
#' }
#'
#' \strong{Confirmatory mode (\code{type = "conf"}):}
#' For each item, its squared loadings are collapsed into two categories: the target factor (given by \code{target}) and the sum of all other factors.
#' Likewise, for each factor, its squared loadings are collapsed into target items (those assigned to that factor) vs. non-target items.
#' Entropy is then computed over these two proportions.
#' \itemize{
#'   \item Item entropy: \eqn{H_i^{conf} = - (p_t \log(p_t) + (1-p_t) \log(1-p_t))}, with \eqn{p_t = \lambda_{i,t}^2 / \sum_j \lambda_{ij}^2}.
#'   \item Factor entropy: \eqn{H_f^{conf} = - (q_t \log(q_t) + (1-q_t) \log(1-q_t))}, with \eqn{q_t = \sum_{i \in target} \lambda_{ij}^2 / \sum_i \lambda_{ij}^2}.
#'   \item Normalization (if \code{normalized = TRUE}) divides by \eqn{\log(2)}.
#' }
#'
#' \strong{Total entropy:}
#' \itemize{
#'   \item \code{H_total_items}: Average entropy across items.
#'   \item \code{H_total_factors}: Average entropy across factors.
#' }
#'
#' \strong{Interpretation:}
#' \itemize{
#'   \item Values near 0 indicate highly simple structures (loadings concentrated in few components).  
#'   \item Values near 1 suggest factorial ambiguity or complexity.  
#'   \item Useful to compare different rotation methods, number of factors, or loading patterns.
#' }
#'
#' @return A list with:
#' \describe{
#'   \item{\code{H_i}}{A numeric vector with the entropy for each item (normalized if requested).}
#'   \item{\code{H_f}}{A numeric vector with the entropy for each factor (normalized if requested).}
#'   \item{\code{H_total_items}}{The average entropy across items.}
#'   \item{\code{H_total_factors}}{The average entropy across factors.}
#'   \item{\code{type}}{Character indicating the mode used.}
#'   \item{\code{normalized}}{Logical indicating whether values are normalized.}
#' }
#'
#' @examples
#' \donttest{
#' # --- Exploratory example ---
#' loadings_expl <- matrix(c(
#'   0.7, 0.0, 0.01,  # simple item
#'   0.1, 0.2, 0.15,  # moderately complex
#'   0.4, 0.8, 0.2,   # complex with a dominant factor
#'   0.4, 0.4, 0.4    # maximally complex (equal loadings)
#' ), nrow = 4, byrow = TRUE)
#' 
#' entropyFL(loadings_expl, type = "expl")
#'
#' # --- Confirmatory example ---
#' # Suppose 3 factors, first 3 items target factor 1, next 2 target factor 2, last 3 target factor 3
#' target_vec <- c(1,1,1, 2,2, 3,3,3)
#' entropyFL(loadings_expl, type = "conf", target = target_vec)
#' }
#'
#' @references
#' Shannon, C. E. (1948). A mathematical theory of communication. \emph{Bell System Technical Journal}, 27(3), 379--423.  
#' Hofmann, R. J. (1978). Complexity and simplicity as objective indices descriptive of factor solutions. \emph{Multivariate Behavioral Research}, 13(2), 247--250.  
#' Lorenzo-Seva, U. (2003). A factor simplicity index. \emph{Psychometrika}, 68(1), 49--60. \doi{10.1007/BF02296652}
#' McCammon, R. B. (1966). Minimum entropy criterion for factor analysis. \emph{Nature}, 211, 146-148.
#'
#' @export
entropyFL <- function(loadings_matrix, 
                      base = 2, 
                      normalized = TRUE, 
                      nd = 3,
                      type = c("expl", "conf"),
                      target = NULL) {
  
  # --- Basic checks ---
  if (!is.matrix(loadings_matrix)) {
    loadings_matrix <- as.matrix(loadings_matrix)
  }
  if (!is.numeric(loadings_matrix)) {
    stop("loadings_matrix must be numeric.")
  }
  
  type <- match.arg(type)
  
  n_items <- nrow(loadings_matrix)
  n_factors <- ncol(loadings_matrix)
  
  if (type == "conf") {
    if (is.null(target)) {
      stop("When type = 'conf', 'target' must be provided.")
    }
    if (length(target) != n_items) {
      stop("'target' must have length equal to number of rows in loadings_matrix.")
    }
    if (any(target < 1 | target > n_factors)) {
      stop("'target' values must be between 1 and ncol(loadings_matrix).")
    }
    # Convert to integer for safety
    target <- as.integer(target)
  }
  
  # --- Squared loadings ---
  load_sq <- loadings_matrix^2
  
  # --- Placeholders for results ---
  H_i <- numeric(n_items)
  H_f <- numeric(n_factors)
  
  # --- Compute entropies ---
  
  if (type == "expl") {
    # ----- EXPLORATORY MODE -----
    
    # Row-wise (items)
    row_sums <- rowSums(load_sq)
    pij <- sweep(load_sq, 1, row_sums, FUN = "/")
    pij[is.nan(pij)] <- 0   # handles zero-sum rows
    H_i <- -rowSums(ifelse(pij > 0, pij * log(pij, base = base), 0), na.rm = TRUE)
    if (normalized) {
      H_i <- H_i / log(n_factors, base = base)
    }
    
    # Column-wise (factors)
    col_sums <- colSums(load_sq)
    qij <- sweep(load_sq, 2, col_sums, FUN = "/")
    qij[is.nan(qij)] <- 0
    H_f <- -colSums(ifelse(qij > 0, qij * log(qij, base = base), 0), na.rm = TRUE)
    if (normalized) {
      H_f <- H_f / log(n_items, base = base)
    }
    
  } else { # type == "conf"
    # ----- CONFIRMATORY MODE -----
    
    # 1) Item entropies: target vs all other factors
    for (i in 1:n_items) {
      t <- target[i]
      p_target <- load_sq[i, t] / sum(load_sq[i, ])   # proportion on target
      # If sum of row is zero, p_target = NaN; handle later
      if (is.nan(p_target) || sum(load_sq[i, ]) == 0) {
        H_i[i] <- NA
        next
      }
      # p_other = 1 - p_target
      p_other <- 1 - p_target
      
      # Entropy for two categories (target vs non-target)
      if (p_target == 0 || p_target == 1) {
        H_i[i] <- 0
      } else {
        H_i[i] <- -(p_target * log(p_target, base = base) + p_other * log(p_other, base = base))
      }
      # Normalize (if requested)
      if (normalized) {
        H_i[i] <- H_i[i] / log(2, base = base)
      }
    }
    
    # 2) Factor entropies: target items vs all other items
    for (j in 1:n_factors) {
      target_items <- which(target == j)
      if (length(target_items) == 0) {
        H_f[j] <- NA
        next
      }
      # Total sum of squares for this factor
      total_ss <- sum(load_sq[, j])
      if (total_ss == 0) {
        H_f[j] <- NA
        next
      }
      target_ss <- sum(load_sq[target_items, j])
      q_target <- target_ss / total_ss
      q_other <- 1 - q_target
      
      if (q_target == 0 || q_target == 1) {
        H_f[j] <- 0
      } else {
        H_f[j] <- -(q_target * log(q_target, base = base) + q_other * log(q_other, base = base))
      }
      if (normalized) {
        H_f[j] <- H_f[j] / log(2, base = base)
      }
    }
  }
  
  # --- Handle cases where n_factors == 1 or n_items == 1 (exploratory) ---
  if (type == "expl") {
    if (n_factors == 1 && normalized) {
      H_i <- rep(0, n_items)
    }
    if (n_items == 1 && normalized) {
      H_f <- rep(0, n_factors)
    }
  }
  
  # --- Totals ---
  H_total_items <- mean(H_i, na.rm = TRUE)
  H_total_factors <- mean(H_f, na.rm = TRUE)
  
  # --- Rounding ---
  if (!is.null(nd) && is.numeric(nd) && nd >= 0) {
    H_i <- round(H_i, nd)
    H_f <- round(H_f, nd)
    H_total_items <- round(H_total_items, nd)
    H_total_factors <- round(H_total_factors, nd)
  }
  
  # --- Output ---
  out <- list(
    H_i = H_i,
    H_f = H_f,
    H_total_items = H_total_items,
    H_total_factors = H_total_factors,
    type = type,
    normalized = normalized
  )
  class(out) <- c("entropyFL", "list")
  return(out)
}