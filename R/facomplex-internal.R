# ===========================================================================
# facomplex — utilidades internas (no exportadas)
# ===========================================================================

`%||%` <- function(a, b) if (is.null(a)) b else a

#' Congruencia de Tucker entre dos vectores
#' @noRd
tuckerCongruence <- function(x, y) {
  den <- sqrt(sum(x^2) * sum(y^2))
  if (den == 0) return(0)
  sum(x * y) / den
}

#' Alineación factorial: congruencia + húngaro + volteo de signo
#' @noRd
alignFactors <- function(L, L0) {
  p <- nrow(L0); r <- ncol(L0)
  if (ncol(L) != r) stop("Column count mismatch in alignment.")
  C <- matrix(0, r, r)
  for (i in seq_len(r)) for (j in seq_len(r))
    C[i, j] <- tuckerCongruence(L[, i], L0[, j])
  
  if (requireNamespace("clue", quietly = TRUE)) {
    perm <- clue::solve_LSAP(abs(C), maximum = TRUE)
  } else {
    perm <- integer(r); used <- logical(r)
    ord  <- order(apply(abs(C), 1, max), decreasing = TRUE)
    for (i in ord) {
      sc <- abs(C[i, ]); sc[used] <- -Inf
      j <- which.max(sc); perm[i] <- j; used[j] <- TRUE
    }
  }
  aligned <- matrix(0, p, r, dimnames = dimnames(L0))
  for (i in seq_len(r)) aligned[, perm[i]] <- L[, i]
  for (j in seq_len(r))
    if (tuckerCongruence(aligned[, j], L0[, j]) < 0) aligned[, j] <- -aligned[, j]
  aligned
}

#' Normalizar target a {binary, rotation, list}
#' @noRd
prepareTarget <- function(target, n_items = NULL) {
  if (is.null(target)) return(NULL)
  if (is.list(target) && !is.data.frame(target) && !is.matrix(target)) {
    if (is.null(n_items)) n_items <- max(unlist(target))
    nf <- length(target)
    binary <- matrix(0L, n_items, nf)
    for (j in seq_along(target)) binary[target[[j]], j] <- 1L
    colnames(binary) <- names(target) %||% paste0("F", seq_len(nf))
    rownames(binary) <- paste0("item", seq_len(n_items))
    rotation <- binary; rotation[rotation == 1] <- NA_integer_
    return(list(binary = binary, rotation = rotation, list = target))
  }
  T <- as.matrix(target)
  if (is.logical(T)) T <- 1L * T
  has_na <- anyNA(T); has_1 <- any(T == 1, na.rm = TRUE)
  if (has_na && !has_1) {
    binary <- T; binary[is.na(binary)] <- 1L
    rotation <- T
  } else {
    binary <- T; binary[is.na(binary)] <- 0L
    rotation <- T; rotation[rotation == 1] <- NA_integer_
  }
  rn <- rownames(T) %||% paste0("item", seq_len(nrow(binary)))
  cn <- colnames(T) %||% paste0("F", seq_len(ncol(binary)))
  rownames(binary) <- rn; colnames(binary) <- cn
  rownames(rotation) <- rn; colnames(rotation) <- cn
  list(binary = binary, rotation = rotation,
       list = stats::setNames(lapply(seq_len(ncol(binary)),
                                     function(j) which(binary[, j] == 1)), cn))
}

#' Re-alinear nombres del target con las cargas ajustadas
#' @noRd
reconcileTarget <- function(tgt, L0) {
  if (is.null(tgt)) return(NULL)
  rownames(tgt$binary) <- rownames(L0); colnames(tgt$binary) <- colnames(L0)
  rownames(tgt$rotation) <- rownames(L0); colnames(tgt$rotation) <- colnames(L0)
  if (is.null(names(tgt$list)) || !all(names(tgt$list) %in% colnames(L0)))
    names(tgt$list) <- colnames(L0)
  tgt
}

#' Ajuste EFA / ESEM unificado
#' @noRd
fitFA <- function(data, engine = "EFA", nfactors = NULL, model = NULL,
                  rotation = "oblimin", rotation.args = list(), ...) {
  dots <- list(...)
  if (!is.null(dots$se) && identical(dots$se, "bootstrap"))
    stop("'se = \"bootstrap\"' es incompatible con el bootstrap externo.")
  if (engine == "EFA") {
    if (is.null(nfactors)) stop("'nfactors' es obligatorio para EFA.")
    do.call(lavaan::efa, c(list(data = data, nfactors = nfactors,
                                rotation = rotation,
                                rotation.args = rotation.args), dots))
  } else if (engine == "ESEM") {
    if (is.null(model)) stop("'model' es obligatorio para ESEM.")
    do.call(lavaan::sem, c(list(model = model, data = data,
                                rotation = rotation,
                                rotation.args = rotation.args), dots))
  } else stop("'engine' debe ser 'EFA' o 'ESEM'.")
}

#' Extraer cargas estandarizadas
#' @noRd
extractLoadings <- function(fit) {
  # Caso 1: EFA (devuelve efaList)
  if (inherits(fit, "efaList")) {
    # En lavaan, fit[[n]] extrae directamente la matriz rotada del modelo de n factores
    L <- fit[[length(fit)]] 
    L <- as.matrix(L)
  } else {
    # Caso 2: ESEM o SEM (devuelve un objeto lavaan normal)
    L <- lavaan::lavInspect(fit, "std")$lambda
    L <- as.matrix(L)
  }
  
  storage.mode(L) <- "double"
  
  if (is.null(rownames(L)) || all(rownames(L) == ""))
    rownames(L) <- paste0("item", seq_len(nrow(L)))
  if (is.null(colnames(L)) || all(colnames(L) == ""))
    colnames(L) <- paste0("F", seq_len(ncol(L)))
  
  L
}

#' IC asimétrico (percentil o bias-corrected)
#' @noRd
computeCI <- function(boot_vals, theta_hat, conf.level = 0.95,
                      type = c("perc", "bc")) {
  type <- match.arg(type)
  v <- boot_vals[!is.na(boot_vals)]
  if (length(v) < 10) return(c(lwr = NA_real_, upr = NA_real_))
  alpha <- 1 - conf.level
  if (type == "perc") {
    q <- stats::quantile(v, c(alpha/2, 1 - alpha/2), names = FALSE)
    return(c(lwr = q[1], upr = q[2]))
  }
  p0 <- mean(v < theta_hat)
  p0 <- min(max(p0, 1e-6), 1 - 1e-6)
  z0 <- stats::qnorm(p0)
  a1 <- stats::pnorm(2 * z0 + stats::qnorm(alpha / 2))
  a2 <- stats::pnorm(2 * z0 + stats::qnorm(1 - alpha / 2))
  q  <- stats::quantile(v, c(a1, a2), names = FALSE)
  c(lwr = q[1], upr = q[2])
}

#' Constructor de fila de salida
#' @noRd
buildRow <- function(level, unit, index, estimate, ci, conf.level, type) {
  data.frame(
    level      = level,
    unit       = unit,
    index      = index,
    estimate   = as.numeric(estimate),
    lwr.ci     = unname(ci["lwr"]),
    upr.ci     = unname(ci["upr"]),
    conf.level = conf.level,
    type       = type,
    stringsAsFactors = FALSE
  )
}

#' @export
print.facomplexBoot <- function(x, ...) {
  cat("<facomplexBoot>\n")
  cat(sprintf("  Engine: %s | Rotacion: %s | Items: %d | Factores: %d\n",
              x$engine, x$rotation,
              nrow(x$loadings_original), x$nfactors))
  cat(sprintf("  Replicas validas: %d / %d\n", x$R_valid, x$R))
  cat(sprintf("  IC: %s | nivel de confianza: %.2f\n",
              x$type, x$conf.level))
  invisible(x)
}