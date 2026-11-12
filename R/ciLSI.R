#' IC bootstrap para el Loading Simplicity Index (LSI)
#'
#' @description
#' Intervalos de confianza asimetricos para el indice global de simplicidad
#' de Lorenzo-Seva (2003), a partir de un objeto \code{facomplexBoot}.
#'
#' @param boot_result Objeto \code{facomplexBoot}.
#' @param conf.level Nivel de confianza. Si \code{NULL}, hereda.
#' @param type \code{"perc"} o \code{"bc"}. Si \code{NULL}, hereda.
#'
#' @return \code{data.frame} con una fila (\code{level = "matrix"},
#'   \code{unit = "global"}).
#'
#' @seealso \code{\link{bootLoadings}}, \code{\link[facomplex]{LSIglobal}}
#'
#' @examples
#' \donttest{
#' data(HolzingerSwineford1939, package = "lavaan")
#' d <- HolzingerSwineford1939[, paste0("x", 1:9)]
#' br <- bootLoadings(d, nfactors = 3, R = 50)
#' ciLSI(br)
#' }
#' @export
ciLSI <- function(boot_result, conf.level = NULL, type = NULL) {
  if (!inherits(boot_result, "facomplexBoot"))
    stop("'boot_result' debe venir de bootLoadings().")
  conf.level <- conf.level %||% boot_result$conf.level
  type       <- type       %||% boot_result$type
  
  L0 <- boot_result$loadings_original
  Lb <- boot_result$loadings_boot
  R  <- boot_result$R; p <- nrow(L0); r <- ncol(L0)
  
  est  <- LSIglobal(L0)
  vals <- vapply(seq_len(R), function(i) {
    L <- Lb[i, , ] # <--- El cambio
    if (anyNA(L)) return(NA_real_)
    tryCatch(BSI(as.data.frame(L)), error = function(e) NA_real_) # o LSIglobal(L)
  }, numeric(1))
  
  ci  <- computeCI(vals, est, conf.level, type)
  out <- buildRow("matrix", "global", "LSI", est, ci, conf.level, type)
  attr(out, "boot_result") <- boot_result
  out
}