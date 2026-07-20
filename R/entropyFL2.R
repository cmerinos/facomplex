#' Entropy Index for Factor Simplicity
#'
#' @description
#' Computes entropy‑based indices to quantify factorial simplicity or complexity
#' from a matrix of factor loadings (or network loadings from EGA).
#' Two modes are available: exploratory (\code{type = "expl"}) and confirmatory
#' (\code{type = "conf"}). In confirmatory mode, a target vector must be provided
#' indicating the expected factor for each item; entropy is then computed on a
#' binary distribution (target vs. non‑target variance).
#'
#' The function returns entropy values per item, per factor, and globally.
#' Entropy can be normalized (divided by the maximum possible entropy) to
#' obtain values between 0 and 1.
#'
#' @param loadings_matrix A numeric matrix of factor loadings (rows = items, columns = factors).
#' @param base Logarithmic base (default \code{2}, entropy in bits).
#' @param normalized Logical. If \code{TRUE}, entropy is divided by the maximum
#'        possible entropy: for items \eqn{\log(k)} (or \eqn{\log(2)} in conf mode),
#'        for factors \eqn{\log(n)} (or \eqn{\log(2)} in conf mode),
#'        and for global \eqn{\log(n \cdot k)}. Default is \code{TRUE}.
#' @param type Character. Either \code{"expl"} (exploratory, default) or \code{"conf"} (confirmatory).
#' @param target Integer vector of length \code{nrow(loadings_matrix)}. Required when
#'        \code{type = "conf"}. Each entry indicates the factor (column index) that
#'        the item is expected to load on. Values must be between 1 and number of factors.
#'
#' @details
#' In exploratory mode, entropy for an item is computed from the normalized squared
#' loadings across all factors. In confirmatory mode, the squared loadings are
#' collapsed into two categories: the target factor and all others combined.
#' Similarly for factors: target items vs. non‑target items.
#'
#' The global entropy is computed on the entire matrix of squared loadings:
#' \deqn{p_{ij} = \frac{\lambda_{ij}^2}{\sum_i \sum_j \lambda_{ij}^2}}
#' \deqn{H_{global} = -\sum_i \sum_j p_{ij} \log(p_{ij})}
#' and normalized by \eqn{\log(n \cdot k)} if \code{normalized = TRUE}.
#'
#' \strong{Interpretation:}
#' \itemize{
#'   \item \strong{Item entropy (\code{H_i})}: Low values (near 0) indicate that the item loads predominantly on a single factor (simple structure). High values (near 1) suggest cross‑loadings or factorial complexity.
#'   \item \strong{Factor entropy (\code{H_f})}: Low values indicate that the factor is defined by few items (specific factor). High values suggest that the factor is broadly dispersed across many items (general or diffuse factor).
#'   \item \strong{Global entropy (\code{H_total})}: Reflects the overall dispersion of variance across all matrix cells. A value near 0 indicates that variance is concentrated in very few cells (e.g., a sparse loading matrix); a value near 1 suggests a uniform distribution across all cells. This measure is not directly comparable to the item or factor entropies because it aggregates across all cells.
#' }
#'
#' When applied to network loadings (NT) from EGA, this index quantifies the
#' clarity of node‑community assignment. Low entropy indicates that nodes load
#' predominantly on a single community (simple structure), high entropy suggests
#' diffuse or ambiguous membership, complementing the Total Entropy Fit Index (TEFI).
#'
#' @return A list of data frames:
#' \describe{
#'   \item{\code{item}}{Data frame with columns \code{item} (names) and \code{entropy}.}
#'   \item{\code{factor}}{Data frame with columns \code{factor} (names) and \code{entropy}.}
#'   \item{\code{total}}{Data frame with a single row: \code{metric = "global"} and \code{entropy}.}
#' }
#' If \code{normalized = FALSE}, the raw entropy values are returned; otherwise,
#' normalized values (0–1) are returned.
#'
#' @references
#' Shannon, C. E. (1948). A mathematical theory of communication. \emph{Bell System Technical Journal}, 27(3), 379–423.
#' Hofmann, R. J. (1978). Complexity and simplicity as objective indices descriptive of factor solutions. \emph{Multivariate Behavioral Research}, 13(2), 247–250.
#' Lorenzo-Seva, U. (2003). A factor simplicity index. \emph{Psychometrika}, 68(1), 49–60.
#' McCammon, R. B. (1966). Minimum entropy criterion for factor analysis. \emph{Journal of the ACM}, 13(2), 247–250.
#'
#' @examples
#' \donttest{
#' # Exploratory mode
#' loadings_expl <- matrix(c(0.7,0.1,0.1, 0.2,0.8,0.1, 0.3,0.3,0.9), nrow=3, byrow=TRUE)
#' rownames(loadings_expl) <- paste0("Item", 1:3)
#' colnames(loadings_expl) <- paste0("F", 1:3)
#'
#' entropyFL(loadings_expl, type = "expl", normalized = TRUE)
#'
#' # Confirmatory mode
#' loadings_conf <- loadings_expl
#' target <- c(1, 2, 3)
#' entropyFL(loadings_conf, type = "conf", target = target, normalized = FALSE)
#' }
#'
#' @export
entropyFL2 <- function(loadings_matrix,
                      base = 2,
                      normalized = TRUE,
                      type = c("expl", "conf"),
                      target = NULL) {
  
  # ---- Validaciones ----
  if (!is.matrix(loadings_matrix)) stop("'loadings_matrix' must be a matrix.")
  n_items <- nrow(loadings_matrix)
  n_factors <- ncol(loadings_matrix)
  if (n_items < 1 || n_factors < 1) stop("Matrix must have at least one row and one column.")
  
  type <- match.arg(type)
  
  if (type == "conf") {
    if (is.null(target)) stop("For type='conf', 'target' must be provided.")
    if (!is.numeric(target) || length(target) != n_items) {
      stop("'target' must be a numeric vector of length nrow(loadings_matrix).")
    }
    if (any(target < 1 | target > n_factors)) {
      stop("'target' values must be between 1 and ncol(loadings_matrix).")
    }
  }
  
  # ---- Obtener nombres ----
  item_names <- rownames(loadings_matrix)
  if (is.null(item_names) || all(item_names == "")) {
    item_names <- paste0("Item", seq_len(n_items))
  }
  factor_names <- colnames(loadings_matrix)
  if (is.null(factor_names) || all(factor_names == "")) {
    factor_names <- paste0("F", seq_len(n_factors))
  }
  
  # ---- Cargas al cuadrado ----
  load_sq <- loadings_matrix^2
  
  # ---- Funciones auxiliares ----
  entropy_vec <- function(p, base) {
    p <- p[p > 0]
    if (length(p) == 0) return(0)
    -sum(p * log(p, base = base))
  }
  
  entropy_binary <- function(p, base) {
    if (p <= 0 || p >= 1) return(0)
    - (p * log(p, base) + (1 - p) * log(1 - p, base))
  }
  
  # ---- Entropía global (siempre sobre toda la matriz) ----
  total_ssq <- sum(load_sq)
  if (total_ssq == 0) {
    H_global_raw <- 0
    H_global <- 0
  } else {
    p_global <- load_sq / total_ssq
    H_global_raw <- entropy_vec(as.vector(p_global), base)
    H_max_global <- log(n_items * n_factors, base = base)
    H_global <- if (normalized) H_global_raw / H_max_global else H_global_raw
  }
  
  # ---- Modo EXPLORATORIO ----
  if (type == "expl") {
    # ---- Entropía por ítem ----
    row_sums <- rowSums(load_sq)
    p_items <- sweep(load_sq, 1, row_sums, FUN = "/")
    p_items[is.nan(p_items)] <- 0
    H_i_raw <- apply(p_items, 1, entropy_vec, base = base)
    H_max_items <- log(n_factors, base = base)
    H_i <- if (normalized) H_i_raw / H_max_items else H_i_raw
    
    # ---- Entropía por factor ----
    col_sums <- colSums(load_sq)
    p_factors <- sweep(load_sq, 2, col_sums, FUN = "/")
    p_factors[is.nan(p_factors)] <- 0
    H_f_raw <- apply(p_factors, 2, entropy_vec, base = base)
    H_max_factors <- log(n_items, base = base)
    H_f <- if (normalized) H_f_raw / H_max_factors else H_f_raw
    
  } else { # ---- Modo CONFIRMATORIO ----
    # ---- Entropía por ítem (target vs resto) ----
    H_i_raw <- numeric(n_items)
    for (i in 1:n_items) {
      target_col <- target[i]
      sq_target <- load_sq[i, target_col]
      sq_other <- sum(load_sq[i, -target_col])
      total <- sq_target + sq_other
      if (total == 0) {
        H_i_raw[i] <- 0
      } else {
        p_t <- sq_target / total
        H_i_raw[i] <- entropy_binary(p_t, base)
      }
    }
    H_max_items <- log(2, base = base)
    H_i <- if (normalized) H_i_raw / H_max_items else H_i_raw
    
    # ---- Entropía por factor (ítems target vs no-target) ----
    H_f_raw <- numeric(n_factors)
    for (j in 1:n_factors) {
      target_items <- which(target == j)
      non_target_items <- setdiff(seq_len(n_items), target_items)
      sq_target <- sum(load_sq[target_items, j])
      sq_other <- sum(load_sq[non_target_items, j])
      total <- sq_target + sq_other
      if (total == 0) {
        H_f_raw[j] <- 0
      } else {
        p_t <- sq_target / total
        H_f_raw[j] <- entropy_binary(p_t, base)
      }
    }
    H_max_factors <- log(2, base = base)
    H_f <- if (normalized) H_f_raw / H_max_factors else H_f_raw
  }
  
  # ---- Construcción de la salida en data.frames ----
  result <- list()
  
  result$item <- data.frame(
    item = item_names,
    entropy = round(H_i, 3),
    stringsAsFactors = FALSE
  )
  
  result$factor <- data.frame(
    factor = factor_names,
    entropy = round(H_f, 3),
    stringsAsFactors = FALSE
  )
  
  result$total <- data.frame(
    metric = "global",
    entropy = round(H_global, 3),
    stringsAsFactors = FALSE
  )
  
  return(result)
}