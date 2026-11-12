#' Bootstrap de cargas factoriales para indices de facomplex
#'
#' @description
#' Ejecuta un bootstrap no parametrico sobre las cargas factoriales de un
#' modelo EFA o ESEM estimado con \pkg{lavaan}. Cada replica se alinea con la
#' solucion original mediante congruencia de Tucker y asignacion optima por
#' algoritmo hungaro, resolviendo la indeterminacion rotacional. El objeto
#' resultante se pasa a las funciones \code{ci*} del paquete para obtener
#' intervalos de confianza asimetricos de los indices de simplicidad.
#'
#' @param data \code{data.frame} con los items (filas = sujetos,
#'   columnas = items).
#' @param engine \code{"EFA"} o \code{"ESEM"}.
#' @param nfactors Numero de factores. Obligatorio si \code{engine = "EFA"}.
#' @param model Sintaxis del modelo. Obligatorio si \code{engine = "ESEM"}.
#' @param target Estructura objetivo. Acepta: (a) matriz 0/1 (1 = carga
#'   esperada, 0 = cruzada); (b) matriz 0/NA (convencion \pkg{lavaan}:
#'   NA = carga libre, 0 = cruzada fija); (c) lista nombrada de indices por
#'   factor (formato \code{items_target} de \code{\link[facomplex]{simload}}).
#'   Si \code{NULL}, no se aplica rotacion target.
#' @param rotation Tipo de rotacion para \pkg{lavaan}. Si \code{target} no es
#'   \code{NULL}, por defecto \code{"target"}. Si es \code{NULL} y no hay
#'   target, por defecto \code{"oblimin"} (EFA) o \code{"geomin"} (ESEM).
#' @param rotation.args Lista de argumentos adicionales para la rotacion.
#' @param R Numero de replicas bootstrap. Por defecto 1000.
#' @param conf.level Nivel de confianza para los IC. Por defecto 0.95.
#' @param type Tipo de IC asimetrico: \code{"perc"} (percentil) o
#'   \code{"bc"} (bias-corrected). Por defecto \code{"perc"}.
#' @param parallel,ncpus Control de paralelizacion; ver
#'   \code{\link[boot]{boot}}.
#' @param progress Logico. Si \code{TRUE}, muestra progreso cada 10\\%.
#' @param align Logico. Si \code{TRUE} (recomendado), alinea las cargas
#'   bootstrap con la solucion original. Ver \strong{Detalles}.
#' @param ... Argumentos adicionales para \code{lavaan::efa} o
#'   \code{lavaan::sem} (p. ej. \code{estimator}, \code{ordered}, \code{missing}).
#'
#' @return Objeto de clase \code{facomplexBoot} con:
#' \describe{
#'   \item{\code{loadings_original}}{Matriz p x r de cargas en la muestra completa.}
#'   \item{\code{loadings_boot}}{Array R x p x r de cargas alineadas por replica.}
#'   \item{\code{boot}}{Objeto \code{boot} de \pkg{boot}.}
#'   \item{\code{R}, \code{R_valid}}{Replicas solicitadas y validas.}
#'   \item{\code{conf.level}, \code{type}}{Parametros de los IC.}
#'   \item{\code{engine}, \code{rotation}, \code{target}, \code{nfactors}}{Metadatos.}
#' }
#'
#' @details
#' \strong{Alineacion factorial.} Cada replica bootstrap puede devolver los
#' factores en un orden distinto o con signo opuesto. Si no se corrige, la
#' variabilidad entre replicas se infla artificialmente y los IC de indices
#' como \code{\link[facomplex]{HofmannFac}} o \code{simload::SFI} se calculan
#' comparando columnas no comparables. La alineacion consta de tres pasos:
#' \enumerate{
#'   \item Congruencia de Tucker entre cada columna bootstrap y cada columna
#'     original.
#'   \item Asignacion optima por algoritmo hungaro (\code{clue::solve_LSAP})
#'     sobre \code{-abs(congruencia)}.
#'   \item Volteo de signo para congruencia positiva con la solucion original.
#' }
#' La alineacion corrige permutacion y reflexion, pero no optimos locales de
#' la rotacion. Si una replica converge a una estructura genuinamente distinta,
#' la congruencia sera baja y el IC lo reflejara (ancho). Esto es informacion
#' valida, no un error.
#'
#' @references
#' Efron, B., & Tibshirani, R. J. (1993). \emph{An Introduction to the
#' Bootstrap}. Chapman & Hall.
#'
#' @seealso \code{\link{ciHofmann}}, \code{\link{ciBSI}}, \code{\link{ciLSI}},
#'   \code{\link{ciSimload}}, \code{\link{ciSSindices}}
#'
#' @examples
#' \donttest{
#' data(HolzingerSwineford1939, package = "lavaan")
#' d <- HolzingerSwineford1939[, paste0("x", 1:9)]
#'
#' # Sin target: rotacion libre + alineacion automatica
#' br <- bootLoadings(d, nfactors = 3, R = 50)
#' print(br)
#'
#' # Con target (convencion 0/NA de lavaan)
#' target <- matrix(0, 9, 3)
#' target[1:3, 1] <- NA; target[4:6, 2] <- NA; target[7:9, 3] <- NA
#' br <- bootLoadings(d, nfactors = 3, target = target, R = 50)
#'
#' # ESEM
#' model <- 'efa("efa1")*f1 + efa("efa1")*f2 + efa("efa1")*f3 =~
#'           x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9'
#' br <- bootLoadings(d, engine = "ESEM", model = model, target = target,
#'                    R = 50)
#' }
#' @export
bootLoadings <- function(
    data,
    engine = c("EFA", "ESEM"),
    nfactors = NULL,
    model = NULL,
    target = NULL,
    rotation = NULL,
    rotation.args = list(),
    R = 1000,
    conf.level = 0.95,
    type = c("perc", "bc"),
    parallel = c("no", "multicore", "snow"),
    ncpus = 1,
    progress = TRUE,
    align = TRUE,
    ...
) {
  engine   <- match.arg(engine)
  type     <- match.arg(type)
  parallel <- match.arg(parallel)
  
  tgt <- prepareTarget(target, n_items = nrow(data))
  if (!is.null(tgt)) {
    if (is.null(nfactors) && engine == "EFA") nfactors <- ncol(tgt$binary)
    if (is.null(rotation)) rotation <- "target"
    rotation.args <- utils::modifyList(rotation.args,
                                       list(target = tgt$rotation))
  } else if (is.null(rotation)) {
    rotation <- if (engine == "EFA") "oblimin" else "geomin"
  }
  
  fit_args <- list(data = data, engine = engine, nfactors = nfactors,
                   model = model, rotation = rotation,
                   rotation.args = rotation.args, ...)
  
  if (progress) message("Ajustando el modelo original...")
  fit0 <- do.call(fitFA, fit_args)
  L0 <- extractLoadings(fit0)
  p  <- nrow(L0); r <- ncol(L0)
  if (!is.null(tgt)) tgt <- reconcileTarget(tgt, L0)
  
  stat_fn <- function(d_b, idx) {
    # 1. Extraer la muestra bootstrap y limpiar nombres de fila (crucial para lavaan)
    d_sample <- d_b[idx, , drop = FALSE]
    rownames(d_sample) <- NULL
    
    out <- tryCatch({
      f <- do.call(fitFA, utils::modifyList(
        fit_args, list(data = d_sample)))
      L <- extractLoadings(f)
      if (align) L <- alignFactors(L, L0)
      as.vector(L)
    }, error = function(e) {
      # 2. Imprimir el error real en la consola para saber qué pasa
      message("Error en replica bootstrap: ", conditionMessage(e))
      rep(NA_real_, p * r)
    })
    
    if (length(out) != p * r) rep(NA_real_, p * r) else out
  }
  
  if (progress && parallel == "no") {
    counter <- new.env(); counter$i <- 0L
    counter$freq <- max(1L, as.integer(R / 10))
    stat_wrapper <- function(d_b, idx) {
      counter$i <- counter$i + 1L
      if (counter$i %% counter$freq == 0L)
        message(sprintf("  Bootstrap: %d/%d (%.0f%%)",
                        counter$i, R, 100 * counter$i / R))
      stat_fn(d_b, idx)
    }
    message(sprintf("Iniciando bootstrap (R = %d)...", R))
    b <- boot::boot(data = data, statistic = stat_wrapper, R = R,
                    parallel = "no")
  } else {
    b <- boot::boot(data = data, statistic = stat_fn, R = R,
                    parallel = parallel, ncpus = ncpus)
  }
  
  L_boot <- array(NA_real_, dim = c(R, p, r),
                  dimnames = list(NULL, rownames(L0), colnames(L0)))
  for (i in seq_len(R)) {
    v <- b$t[i, ]
    if (!anyNA(v)) L_boot[i, , ] <- matrix(v, p, r)
  }
  n_fail <- sum(is.na(L_boot[, 1, 1]))
  if (n_fail > 0)
    warning(sprintf("%d/%d replicas fallaron o devolvieron NA.",
                    n_fail, R))
  
  structure(
    list(
      loadings_original = L0,
      loadings_boot     = L_boot,
      boot              = b,
      R                 = R,
      R_valid           = R - n_fail,
      conf.level        = conf.level,
      type              = type,
      engine            = engine,
      rotation          = rotation,
      target            = tgt,
      nfactors          = r
    ),
    class = "facomplexBoot"
  )
}