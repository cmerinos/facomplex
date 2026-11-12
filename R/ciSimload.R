#' IC bootstrap para los indices de Fleming (simload)
#'
#' @description
#' Intervalos de confianza asimetricos para \code{TSFI} (nivel matriz),
#' \code{SFI} (nivel factor) e \code{IFS} (nivel item), calculados con
#' \code{\link[facomplex]{simload}} sobre las cargas bootstrap.
#'
#' @param boot_result Objeto \code{facomplexBoot}. Debe haberse construido
#'   con un \code{target} (ver \code{\link{bootLoadings}}).
#' @param conf.level Nivel de confianza. Si \code{NULL}, hereda.
#' @param type \code{"perc"} o \code{"bc"}. Si \code{NULL}, hereda.
#'
#' @return \code{data.frame} largo con filas para matriz (TSFI), factores
#'   (SFI) e items (IFS).
#'
#' @details
#' El \code{items_target} se toma del \code{target} guardado en
#' \code{boot_result} y se mantiene fijo en todas las replicas: la definicion
#' de que items pertenecen a que factor es parte de la hipotesis estructural,
#' no algo que deba variar bajo remuestreo.
#'
#' @seealso \code{\link{bootLoadings}}, \code{\link[facomplex]{simload}}
#'
#' @examples
#' \donttest{
#' data(HolzingerSwineford1939, package = "lavaan")
#' d <- HolzingerSwineford1939[, paste0("x", 1:9)]
#' target <- matrix(0, 9, 3)
#' target[1:3, 1] <- NA; target[4:6, 2] <- NA; target[7:9, 3] <- NA
#' br <- bootLoadings(d, nfactors = 3, target = target, R = 50)
#' ciSimload(br)
#' }
#' @export
ciSimload <- function(boot_result, conf.level = NULL, type = NULL) {
  if (!inherits(boot_result, "facomplexBoot"))
    stop("'boot_result' debe venir de bootLoadings().")
  if (is.null(boot_result$target))
    stop("'ciSimload()' requiere un 'target' en bootLoadings().")
  conf.level <- conf.level %||% boot_result$conf.level
  type       <- type       %||% boot_result$type
  
  L0 <- boot_result$loadings_original
  Lb <- boot_result$loadings_boot
  R  <- boot_result$R; p <- nrow(L0); r <- ncol(L0)
  
  items_target <- boot_result$target$list
  if (is.null(names(items_target)) ||
      !all(names(items_target) %in% colnames(L0)))
    names(items_target) <- colnames(L0)
  
  est <- simload(as.data.frame(L0), items_target = items_target)
  boot_list <- lapply(seq_len(R), function(i) {
    L <- Lb[i, , ] # <--- Aquí está el cambio (extrae la matriz conservando nombres)
    if (anyNA(L)) return(NULL)
    tryCatch(
      simload(as.data.frame(L), items_target = items_target), # Quitamos facomplex::
      error = function(e) NULL)
  })
  
  rows <- list()
  
  # TSFI (matriz)
  tsfi_boot <- vapply(boot_list,
                      function(x) if (is.null(x)) NA_real_ else x$TSFI,
                      numeric(1))
  rows[[length(rows) + 1L]] <- buildRow(
    "matrix", "global", "TSFI", est$TSFI,
    computeCI(tsfi_boot, est$TSFI, conf.level, type), conf.level, type)
  
  # SFI (factor)
  fnames  <- names(est$SFI) %||% colnames(L0)
  sfi_mat <- do.call(rbind, lapply(boot_list, function(x) {
    if (is.null(x)) return(rep(NA_real_, length(fnames)))
    v <- x$SFI; v[match(fnames, names(v))]
  }))
  for (j in seq_along(fnames)) {
    est_j <- est$SFI[[fnames[j]]]
    rows[[length(rows) + 1L]] <- buildRow(
      "factor", fnames[j], "SFI", est_j,
      computeCI(sfi_mat[, j], est_j, conf.level, type), conf.level, type)
  }
  
  # IFS (item)
  ifs_names <- est$IFS$Items
  ifs_mat <- do.call(rbind, lapply(boot_list, function(x) {
    if (is.null(x)) return(rep(NA_real_, length(ifs_names)))
    x$IFS$IFS[match(ifs_names, x$IFS$Items)]
  }))
  for (j in seq_along(ifs_names)) {
    est_j <- est$IFS$IFS[j]
    rows[[length(rows) + 1L]] <- buildRow(
      "item", ifs_names[j], "IFS", est_j,
      computeCI(ifs_mat[, j], est_j, conf.level, type), conf.level, type)
  }
  
  out <- do.call(rbind, rows); rownames(out) <- NULL
  attr(out, "boot_result") <- boot_result
  out
}