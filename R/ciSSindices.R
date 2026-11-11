#' IC bootstrap para SSindices (SStarget, SSntarget, SSratio)
#'
#' @description
#' Intervalos de confianza asimetricos para los indices de simplicidad
#' basados en target de \code{\link[facomplex]{SSindices}}, a nivel global
#' o por factor, a partir de un objeto \code{facomplexBoot}.
#'
#' @param boot_result Objeto \code{facomplexBoot}. Debe haberse construido
#'   con un \code{target}.
#' @param per.factor Logico. Si \code{TRUE}, devuelve resultados por factor.
#' @param conf.level Nivel de confianza. Si \code{NULL}, hereda.
#' @param type \code{"perc"} o \code{"bc"}. Si \code{NULL}, hereda.
#'
#' @return \code{data.frame} largo. Si \code{per.factor = FALSE}, filas
#'   \code{level = "matrix"}; si \code{per.factor = TRUE}, filas
#'   \code{level = "factor"}.
#'
#' @seealso \code{\link{bootLoadings}}, \code{\link[facomplex]{SSindices}}
#'
#' @examples
#' \donttest{
#' data(HolzingerSwineford1939, package = "lavaan")
#' d <- HolzingerSwineford1939[, paste0("x", 1:9)]
#' target <- matrix(0, 9, 3)
#' target[1:3, 1] <- 1; target[4:6, 2] <- 1; target[7:9, 3] <- 1
#' br <- bootLoadings(d, nfactors = 3, target = target, R = 50)
#'
#' ciSSindices(br)
#' ciSSindices(br, per.factor = TRUE)
#' }
#' @export
ciSSindices <- function(boot_result, per.factor = FALSE,
                        conf.level = NULL, type = NULL) {
  if (!inherits(boot_result, "facomplexBoot"))
    stop("'boot_result' debe venir de bootLoadings().")
  if (is.null(boot_result$target))
    stop("'ciSSindices()' requiere un 'target' en bootLoadings().")
  conf.level <- conf.level %||% boot_result$conf.level
  type       <- type       %||% boot_result$type
  
  L0 <- boot_result$loadings_original
  Lb <- boot_result$loadings_boot
  R  <- boot_result$R; p <- nrow(L0); r <- ncol(L0)
  binary <- boot_result$target$binary
  rownames(binary) <- rownames(L0)
  colnames(binary) <- colnames(L0)
  
  est <- SSindices(L0, binary, per.factor = per.factor)
  boot_list <- lapply(seq_len(R), function(i) {
    L <- matrix(Lb[i, , ], p, r)
    if (anyNA(L)) return(NULL)
    tryCatch(SSindices(L, binary, per.factor = per.factor),
             error = function(e) NULL)
  })
  
  rows <- list()
  if (!per.factor) {
    for (col in names(est)) {
      est_j <- est[[col]]
      v <- vapply(boot_list,
                  function(df) if (is.null(df)) NA_real_ else df[[col]],
                  numeric(1))
      rows[[length(rows) + 1L]] <- buildRow(
        "matrix", "global", col, est_j,
        computeCI(v, est_j, conf.level, type), conf.level, type)
    }
  } else {
    fnames <- rownames(est) %||% colnames(L0)
    for (col in names(est)) {
      mat <- do.call(rbind, lapply(boot_list, function(df) {
        if (is.null(df) || !col %in% names(df)) return(rep(NA_real_, r))
        v <- df[[col]]; if (length(v) != r) rep(NA_real_, r) else v
      }))
      for (k in seq_len(r)) {
        est_k <- est[[col]][k]
        rows[[length(rows) + 1L]] <- buildRow(
          "factor", fnames[k], col, est_k,
          computeCI(mat[, k], est_k, conf.level, type), conf.level, type)
      }
    }
  }
  
  out <- do.call(rbind, rows); rownames(out) <- NULL
  attr(out, "boot_result") <- boot_result
  out
}