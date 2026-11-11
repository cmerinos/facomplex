#' IC bootstrap para el indice de Hofmann (CHof)
#'
#' @description
#' Calcula intervalos de confianza asimetricos para el indice de complejidad
#' de Hofmann (1977) a nivel de item y/o de factor, a partir de un objeto
#' \code{facomplexBoot} generado por \code{\link{bootLoadings}}.
#'
#' @param boot_result Objeto \code{facomplexBoot}.
#' @param level \code{"item"}, \code{"factor"} o \code{"both"}.
#' @param scaledVersion Logico. Anade \code{CHof.R} (complejidad 0-1).
#' @param simplVersion Logico. Anade \code{CHof.S} (simplicidad 0-1).
#' @param includeMean Logico. Anade una fila \code{unit = "mean"} con la
#'   media de los items (solo nivel item).
#' @param conf.level Nivel de confianza. Si \code{NULL}, hereda de
#'   \code{boot_result}.
#' @param type \code{"perc"} o \code{"bc"}. Si \code{NULL}, hereda.
#'
#' @return \code{data.frame} con columnas \code{level}, \code{unit},
#'   \code{index}, \code{estimate}, \code{lwr.ci}, \code{upr.ci},
#'   \code{conf.level}, \code{type}.
#'
#' @details
#' Para cada replica bootstrap se recalcula \code{\link[facomplex]{Hofmann}}
#' (nivel item) o \code{\link[facomplex]{HofmannFac}} (nivel factor). Los IC
#' se construyen con los cuantiles empiricos de las replicas (percentil) o
#' con correccion de sesgo (\code{"bc"}).
#'
#' @seealso \code{\link{bootLoadings}}, \code{\link[facomplex]{Hofmann}},
#'   \code{\link[facomplex]{HofmannFac}}
#'
#' @examples
#' \donttest{
#' data(HolzingerSwineford1939, package = "lavaan")
#' d <- HolzingerSwineford1939[, paste0("x", 1:9)]
#' br <- bootLoadings(d, nfactors = 3, R = 50)
#'
#' ciHofmann(br)
#' ciHofmann(br, level = "factor")
#' ciHofmann(br, level = "both", scaledVersion = TRUE, simplVersion = TRUE)
#' }
#' @export
ciHofmann <- function(boot_result,
                      level = c("item", "factor", "both"),
                      scaledVersion = FALSE,
                      simplVersion = FALSE,
                      includeMean = TRUE,
                      conf.level = NULL,
                      type = NULL) {
  if (!inherits(boot_result, "facomplexBoot"))
    stop("'boot_result' debe venir de bootLoadings().")
  level <- match.arg(level)
  conf.level <- conf.level %||% boot_result$conf.level
  type       <- type       %||% boot_result$type
  
  L0 <- boot_result$loadings_original
  Lb <- boot_result$loadings_boot
  R  <- boot_result$R
  p  <- nrow(L0); r <- ncol(L0)
  item_names   <- rownames(L0)
  factor_names <- colnames(L0)
  
  rows <- list()
  
  if (level %in% c("item", "both")) {
    est <- facomplex::Hofmann(as.data.frame(L0),
                              scaledVersion = scaledVersion,
                              simplVersion = simplVersion)
    boot_list <- lapply(seq_len(R), function(i) {
      L <- matrix(Lb[i, , ], p, r)
      if (anyNA(L)) return(NULL)
      tryCatch(
        facomplex::Hofmann(as.data.frame(L),
                           scaledVersion = scaledVersion,
                           simplVersion = simplVersion),
        error = function(e) NULL)
    })
    
    for (col in names(est)) {
      mat <- do.call(rbind, lapply(boot_list, function(df) {
        if (is.null(df) || !col %in% names(df)) return(rep(NA_real_, p))
        v <- df[[col]]; if (length(v) != p) rep(NA_real_, p) else v
      }))
      for (k in seq_len(p)) {
        ci <- computeCI(mat[, k], est[[col]][k], conf.level, type)
        rows[[length(rows) + 1L]] <- buildRow(
          "item", item_names[k], col, est[[col]][k], ci, conf.level, type)
      }
      if (includeMean) {
        mean_est  <- mean(est[[col]], na.rm = TRUE)
        mean_boot <- apply(mat, 1, mean, na.rm = TRUE)
        ci <- computeCI(mean_boot, mean_est, conf.level, type)
        rows[[length(rows) + 1L]] <- buildRow(
          "item", "mean", col, mean_est, ci, conf.level, type)
      }
    }
  }
  
  if (level %in% c("factor", "both")) {
    est_f <- facomplex::HofmannFac(as.data.frame(L0))
    col <- names(est_f)[1]
    boot_list_f <- lapply(seq_len(R), function(i) {
      L <- matrix(Lb[i, , ], p, r)
      if (anyNA(L)) return(NULL)
      tryCatch(
        facomplex::HofmannFac(as.data.frame(L)),
        error = function(e) NULL)
    })
    mat <- do.call(rbind, lapply(boot_list_f, function(df) {
      if (is.null(df) || !col %in% names(df)) return(rep(NA_real_, r))
      v <- df[[col]]; if (length(v) != r) rep(NA_real_, r) else v
    }))
    for (k in seq_len(r)) {
      ci <- computeCI(mat[, k], est_f[[col]][k], conf.level, type)
      rows[[length(rows) + 1L]] <- buildRow(
        "factor", factor_names[k], col, est_f[[col]][k], ci, conf.level, type)
    }
  }
  
  out <- do.call(rbind, rows); rownames(out) <- NULL
  attr(out, "boot_result") <- boot_result
  out
}