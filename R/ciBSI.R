#' IC bootstrap para el Bentler Simplicity Index (BSI)
#'
#' @description
#' Intervalos de confianza asimetricos para el indice global de simplicidad
#' de Bentler (1977), a partir de un objeto \code{facomplexBoot}.
#'
#' @param boot_result Objeto \code{facomplexBoot}.
#' @param conf.level Nivel de confianza. Si \code{NULL}, hereda.
#' @param type \code{"perc"} o \code{"bc"}. Si \code{NULL}, hereda.
#'
#' @return \code{data.frame} con una fila (\code{level = "matrix"},
#'   \code{unit = "global"}).
#'
#' @seealso \code{\link{bootLoadings}}, \code{\link[facomplex]{BSI}}
#'
#' @examples
#' \donttest{
#' data(HolzingerSwineford1939, package = "lavaan")
#' d <- HolzingerSwineford1939[, paste0("x", 1:9)]
#' br <- bootLoadings(d, nfactors = 3, R = 50)
#' ciBSI(br)
#' }
#' @export
ciBSI <- function(boot_result, conf.level = NULL, type = NULL) {
  if (!inherits(boot_result, "facomplexBoot"))
    stop("'boot_result' debe venir de bootLoadings().")
  conf.level <- conf.level %||% boot_result$conf.level
  type       <- type       %||% boot_result$type
  
  L0 <- boot_result$loadings_original
  Lb <- boot_result$loadings_boot
  R  <- boot_result$R; p <- nrow(L0); r <- ncol(L0)
  
  est  <- facomplex::BSI(as.data.frame(L0))
  vals <- vapply(seq_len(R), function(i) {
    L <- matrix(Lb[i, , ], p, r)
    if (anyNA(L)) return(NA_real_)
    tryCatch(facomplex::BSI(as.data.frame(L)), error = function(e) NA_real_)
  }, numeric(1))
  
  ci  <- computeCI(vals, est, conf.level, type)
  out <- buildRow("matrix", "global", "BSI", est, ci, conf.level, type)
  attr(out, "boot_result") <- boot_result
  out
}