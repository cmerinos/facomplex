#' Bootstrap of Factor Loadings for facomplex Indices
#'
#' @description
#' Executes a non-parametric bootstrap of factor loadings for EFA or ESEM
#' models fitted with \pkg{lavaan}. Each replicate is aligned to the original
#' solution using Tucker's congruence coefficient and optimal assignment via
#' the Hungarian algorithm, thereby resolving rotational indeterminacy. The
#' resulting object is passed to the \code{ci*} functions in the package to
#' obtain asymmetric confidence intervals for the factorial simplicity indices.
#'
#' @param data A \code{data.frame} containing the items (rows = subjects,
#'   columns = items).
#' @param engine Character. Either \code{"EFA"} or \code{"ESEM"}.
#' @param nfactors Integer. Number of factors. Required if \code{engine = "EFA"}.
#' @param model Character string. Model syntax. Required if \code{engine = "ESEM"}.
#' @param target Target structure. Accepts: (a) a 0/1 matrix (1 = expected
#'   loading, 0 = cross-loading); (b) a 0/NA matrix (\pkg{lavaan} convention:
#'   NA = free loading, 0 = fixed cross-loading); (c) a named list of indices
#'   per factor (the \code{items_target} format used by \code{\link{simload}}).
#'   If \code{NULL}, no target rotation is applied.
#' @param rotation Character string indicating the rotation method for
#'   \pkg{lavaan}. If \code{target} is not \code{NULL}, defaults to
#'   \code{"target"}. If \code{NULL} and no target is provided, defaults to
#'   \code{"oblimin"} (EFA) or \code{"geomin"} (ESEM).
#' @param rotation_list A list of additional arguments to pass to the rotation
#'   algorithm (e.g., \code{list(target = Tmat)}).
#' @param R Integer. Number of bootstrap replicates. Defaults to 1000.
#' @param conf.level Numeric. Confidence level for the intervals. Defaults to 0.95.
#' @param type Character. Type of asymmetric confidence interval: \code{"perc"}
#'   (percentile) or \code{"bc"} (bias-corrected). Defaults to \code{"perc"}.
#' @param parallel Character. Controls for parallelization; see \code{\link[boot]{boot}}.
#' @param ncpus Integer. Number of cores for parallelization.
#' @param progress Logical. If \code{TRUE}, displays progress every 10 percent.
#' @param align Logical. If \code{TRUE} (recommended), aligns bootstrap
#'   loadings with the original solution. See \strong{Details}.
#' @param ... Additional arguments passed to \code{lavaan::efa} or
#'   \code{lavaan::sem} (e.g., \code{estimator}, \code{ordered}, \code{missing}).
#'
#' @return An object of class \code{facomplexBoot} containing:
#' \itemize{
#'   \item \code{loadings_original}: A p x r matrix of loadings from the full sample.
#'   \item \code{loadings_boot}: An R x p x r array of aligned bootstrap loadings.
#'   \item \code{boot}: The \code{boot} object from \pkg{boot}.
#'   \item \code{R}, \code{R_valid}: Requested and valid bootstrap replicates.
#'   \item \code{conf.level}, \code{type}: Confidence interval parameters.
#'   \item \code{engine}, \code{rotation}, \code{target}, \code{nfactors}: Metadata.
#' }
#'
#' @details
#' \strong{Factor alignment.} Each bootstrap replicate may return the factors
#' in a different order or with opposite signs. If left uncorrected, between-replicate
#' variability would be artificially inflated, and confidence intervals for indices
#' such as \code{\link{HofmannFac}} or \code{\link{simload}} would be computed by
#' comparing incompatible columns. Alignment involves three steps:
#' \enumerate{
#'   \item Tucker's congruence coefficient between each bootstrap column and
#'     each original column.
#'   \item Optimal assignment via the Hungarian algorithm
#'     (\code{clue::solve_LSAP}) applied to the absolute congruences.
#'   \item Sign reflection to ensure positive congruence with the original solution.
#' }
#' Alignment corrects for permutation and reflection, but not for local optima of
#' the rotation algorithm. If a replicate converges to a genuinely different
#' structure, the congruence will be low, and the confidence interval will reflect
#' this (wider intervals). This is valid information, not an error.
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
#' # Without target: free rotation + automatic alignment
#' br <- bootLoadings(d, nfactors = 3, R = 50)
#' print(br)
#'
#' # With target (lavaan's 0/NA convention)
#' target <- matrix(0, 9, 3)
#' target[1:3, 1] <- NA; target[4:6, 2] <- NA; target[7:9, 3] <- NA
#' br <- bootLoadings(d, nfactors = 3, target = target, R = 50)
#'
#' # ESEM
#' model <- paste0('efa("efa1")*f1 + efa("efa1")*f2 + efa("efa1")*f3 =~ ',
#'                 'x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9')
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
    rotation_list = list(),
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
  
  # Build rotation arguments according to the new lavaan syntax
  if (!is.null(tgt)) {
    if (is.null(nfactors) && engine == "EFA") nfactors <- ncol(tgt$binary)
    if (is.null(rotation)) rotation <- "target"
    rotation_list$target <- tgt$rotation
  } else if (is.null(rotation)) {
    rotation <- if (engine == "EFA") "oblimin" else "geomin"
  }
  
  fit_args <- list(data = data, engine = engine, nfactors = nfactors,
                   model = model, rotation = rotation,
                   rotation_list = rotation_list, ...)
  
  if (progress) message("Fitting the original model...")
  fit0 <- do.call(fitFA, fit_args)
  L0 <- extractLoadings(fit0)
  p  <- nrow(L0); r <- ncol(L0)
  if (!is.null(tgt)) tgt <- reconcileTarget(tgt, L0)
  
  stat_fn <- function(d_b, idx) {
    # 1. Extract the bootstrap sample and clear row names (crucial for lavaan)
    d_sample <- d_b[idx, , drop = FALSE]
    rownames(d_sample) <- NULL
    
    out <- tryCatch({
      f <- do.call(fitFA, utils::modifyList(
        fit_args, list(data = d_sample)))
      L <- extractLoadings(f)
      if (align) L <- alignFactors(L, L0)
      as.vector(L)
    }, error = function(e) {
      # 2. Print the actual error to the console for diagnostics
      message("Error in bootstrap replicate: ", conditionMessage(e))
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
    message(sprintf("Starting bootstrap (R = %d)...", R))
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
    warning(sprintf("%d/%d bootstrap replicates failed or returned NA.",
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