#' @title Profile of Factorial Complexity
#' @description
#' Computes descriptive statistics of factor loadings per factor, distinguishing between target and cross-loadings.
#' Optionally, reports the percentage of cross-loadings below and above a user-defined cutoff.
#'
#' @param loadings A numeric matrix or data.frame of factor loadings. Rows are items and columns are factors.
#' @param target A named list, where each name corresponds to a factor in `loadings`, and each element is a character vector of item names (matching the row names of `loadings`) assigned as target to that factor.
#' @param abs Logical. Should absolute values of loadings be used? Default is TRUE.
#' @param cutoff Optional numeric. If defined, reports the percentage of cross-loadings ≤ and > this value.
#' @param digits Integer. Number of decimal places to round the output. Default is 3.
#'
#' @details
#' The function returns a data.frame where:
#' \itemize{
#'   \item Each row corresponds to a summary statistic.
#'   \item Each column (after the first) corresponds to a factor defined in `target`.
#' }
#'
#' The following statistics are computed per factor:
#' \itemize{
#'   \item \strong{Mean.Target, Median.Target, SD.Target}: Central tendency and dispersion of loadings on the target factor.
#'   \item \strong{Min.Target, Max.Target}: Minimum and maximum of the target loadings.
#'   \item \strong{Mean.Cross, Median.Cross, SD.Cross}: Descriptive statistics of cross-loadings (i.e., loadings on non-target factors).
#'   \item \strong{Min.Cross, Max.Cross}: Minimum and maximum of the cross-loadings.
#'   \item \strong{Perc.Cross.≤.cutoff, Perc.Cross.>.cutoff}: If `cutoff` is specified, these show the percentage of cross-loadings ≤ or > that threshold.
#'   \item \strong{n.Items}: Number of items assigned to each factor.
#' }
#'
#' This orientation (statistics in rows, factors in columns) facilitates interpretation and avoids excessively wide tables when the number of factors is large.
#'
#' @return A data.frame in long format: each row corresponds to a statistic and each column to a factor.
#'
#' @examples
#' # Example with simulated data
#' loadings <- matrix(c(.70, .20,
#'                      .68, .22,
#'                      .15, .75,
#'                      .10, .70), ncol = 2, byrow = TRUE)
#' rownames(loadings) <- c("item1", "item2", "item3", "item4")
#' colnames(loadings) <- c("F1", "F2")
#'
#' target <- list(F1 = c("item1", "item2"),
#'                F2 = c("item3", "item4"))
#'
#' profile.facomplex(loadings, target, cutoff = 0.30)
#'
#' @export
profile.facomplex <- function(loadings, target, abs = TRUE, cutoff = NULL, digits = 3) {
  if (!is.matrix(loadings) && !is.data.frame(loadings)) {
    stop("`loadings` must be a matrix or data.frame.")
  }
  loadings <- as.data.frame(loadings)
  
  # Verifica nombres de fila
  if (is.null(rownames(loadings))) {
    stop("`loadings` must have row names (item names).")
  }
  
  # Verifica que los nombres de factores estén en las columnas
  if (!all(names(target) %in% colnames(loadings))) {
    stop("All names in `target` must match column names in `loadings`.")
  }
  
  # Usa valores absolutos si se indica
  if (abs) loadings <- abs(loadings)
  
  result_list <- list()
  
  for (f in names(target)) {
    items_f <- target[[f]]
    other_factors <- setdiff(colnames(loadings), f)
    
    # Verifica existencia de ítems
    if (!all(items_f %in% rownames(loadings))) {
      stop(paste("Some items for factor", f, "are not found in `loadings`."))
    }
    
    load_f <- loadings[items_f, , drop = FALSE]
    target_vals <- load_f[[f]]
    cross_vals <- as.matrix(load_f[, other_factors, drop = FALSE])
    
    # Estadísticos descriptivos
    stat <- c(
      Mean.Target = mean(target_vals),
      Median.Target = median(target_vals),
      SD.Target = sd(target_vals),
      Mean.Cross = mean(cross_vals),
      Median.Cross = median(cross_vals),
      SD.Cross = sd(cross_vals)
    )
    
    # Si hay cutoff, calcular % por debajo/encima
    if (!is.null(cutoff)) {
      total_cross <- length(cross_vals)
      below <- mean(cross_vals <= cutoff) * 100
      above <- mean(cross_vals > cutoff) * 100
      cutoff.label <- formatC(cutoff, format = "f", digits = 2)
      stat[paste0("Perc.Cross.≤.", cutoff.label)] <- below
      stat[paste0("Perc.Cross.>.", cutoff.label)] <- above
    }
    
    stat["n.Items"] <- length(items_f)
    result_list[[f]] <- round(stat, digits)
  }
  
  # Convertir lista a data.frame largo
  df <- do.call(cbind, result_list)
  stat_names <- rownames(df)
  df <- data.frame(Statistic = stat_names, df, row.names = NULL)
  return(df)
}
