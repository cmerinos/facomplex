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
#' The function returns entropy values per item, per factor, and a global index.
#' Entropy can be normalized (divided by the maximum possible entropy) to
#' obtain values between 0 and 1.
#'
#' In exploratory mode, the global entropy is calculated on the entire matrix of
#' squared loadings, normalized by \eqn{\log(n \cdot k)}.
#' In confirmatory mode, the global entropy is computed as the average of two
#' components: (1) the entropy of the squared loadings of target items within
#' each factor (how concentrated are the expected loadings), and (2) the entropy
#' of the squared loadings of non‑target items within each factor (how dispersed
#' are the cross‑loadings). This provides a measure of overall factorial clarity
#' that aligns with the confirmatory logic.
#'
#' @param loadings_matrix A numeric matrix of factor loadings (rows = items, columns = factors).
#' @param base Logarithmic base (default \code{2}, entropy in bits).
#' @param normalized Logical. If \code{TRUE}, entropy is divided by the maximum
#'        possible entropy: for items \eqn{\log(k)} (or \eqn{\log(2)} in conf mode),
#'        for factors \eqn{\log(n)} (or \eqn{\log(2)} in conf mode),
#'        and for global as described above. Default is \code{TRUE}.
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
#' The global entropy in confirmatory mode is calculated as:
#' \enumerate{
#'   \item For each factor, compute the entropy of the distribution of squared
#'         loadings among its target items (if any). Average these values across factors
#'         to obtain \eqn{H_{target}}.
#'   \item For each factor, compute the entropy of the distribution of squared
#'         loadings among its non‑target items (if any). Average these values across factors
#'         to obtain \eqn{H_{non-target}}.
#'   \item The total confirmatory entropy is the average of the two: \eqn{H_{total} = (H_{target} + H_{non-target}) / 2}.
#' }
#' This measure reflects how well the loading matrix conforms to the expected
#' structure: low values indicate that target items have concentrated loadings
#' and non‑target items have low or dispersed cross‑loadings (good fit), while
#' high values suggest ambiguity or poor discrimination.
#'
#' \strong{Interpretation:}
#' \itemize{
#'   \item \strong{Item entropy (\code{H_i})}: Low values (near 0) indicate that the item loads predominantly on a single factor (simple structure). High values (near 1) suggest cross‑loadings or factorial complexity.
#'   \item \strong{Factor entropy (\code{H_f})}: Low values indicate that the factor is defined by few items (specific factor). High values suggest that the factor is broadly dispersed across many items (general or diffuse factor).
#'   \item \strong{Global entropy (\code{H_total})}: In exploratory mode, reflects the overall dispersion of variance across all matrix cells. In confirmatory mode, it reflects the clarity of the target structure, with lower values indicating better alignment with the expected pattern.
#' }
#'
#' When applied to network loadings (NT) from \strong{Exploratory Graph Analysis (EGA)},
#' this index quantifies the clarity of node–community assignment. Low entropy
#' indicates that nodes load predominantly on a single community (simple structure),
#' high entropy suggests diffuse or ambiguous membership, complementing the
#' \emph{Total Entropy Fit Index (TEFI)} for global fit assessment.
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
  if (!is.data.frame(loadings_matrix)) stop("'loadings_matrix' must be a dataframe.")
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

    # ---- Entropía total (global) ----
    total_ssq <- sum(load_sq)
    if (total_ssq == 0) {
      H_total <- 0
    } else {
      p_global <- load_sq / total_ssq
      H_global_raw <- entropy_vec(as.vector(p_global), base)
      H_max_global <- log(n_items * n_factors, base = base)
      H_total <- if (normalized) H_global_raw / H_max_global else H_global_raw
    }

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

    # ---- Entropía total confirmatoria ----
    # Para cada factor, calcular entropía de target y no-target por separado
    # y luego promediar sobre factores
    H_target_vals <- numeric(n_factors)
    H_non_target_vals <- numeric(n_factors)

    for (j in 1:n_factors) {
      target_items <- which(target == j)
      non_target_items <- setdiff(seq_len(n_items), target_items)

      # Entropía de los ítems target en este factor
      if (length(target_items) > 0) {
        sq_target <- load_sq[target_items, j]
        total_target <- sum(sq_target)
        if (total_target > 0) {
          p_target <- sq_target / total_target
          H_target_raw <- entropy_vec(p_target, base)
          H_max_target <- log(length(target_items), base = base)
          H_target_vals[j] <- if (normalized) H_target_raw / H_max_target else H_target_raw
        } else {
          H_target_vals[j] <- 0
        }
      } else {
        H_target_vals[j] <- NA
      }

      # Entropía de los ítems no-target en este factor
      if (length(non_target_items) > 0) {
        sq_non_target <- load_sq[non_target_items, j]
        total_non_target <- sum(sq_non_target)
        if (total_non_target > 0) {
          p_non_target <- sq_non_target / total_non_target
          H_non_target_raw <- entropy_vec(p_non_target, base)
          H_max_non_target <- log(length(non_target_items), base = base)
          H_non_target_vals[j] <- if (normalized) H_non_target_raw / H_max_non_target else H_non_target_raw
        } else {
          H_non_target_vals[j] <- 0
        }
      } else {
        H_non_target_vals[j] <- NA
      }
    }

    # Promediar sobre factores, omitiendo NA
    mean_target <- mean(H_target_vals, na.rm = TRUE)
    mean_non_target <- mean(H_non_target_vals, na.rm = TRUE)

    # Si algún factor no tiene target o no-target, se lanza un warning
    if (any(is.na(H_target_vals)) || any(is.na(H_non_target_vals))) {
      warning("Some factors have no target or no non-target items. Their entropy was omitted from the average.")
    }

    # Total es el promedio de las dos medias
    H_total <- (mean_target + mean_non_target) / 2
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
    entropy = round(H_total, 3),
    stringsAsFactors = FALSE
  )

  return(result)
}
