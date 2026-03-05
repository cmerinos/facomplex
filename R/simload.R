#' Factor Simplicity indices for total, scale and items
#'
#' Calculates a fit indices to evaluate the factorial simplicity of 
#' multidimensional scales. The function estimates factorial simplicity at the item level, 
#' factor level, and overall solution level. It is particularly useful for solutions that 
#' include expected cross-loadings. The approach used in this function is when there is prior 
#' knowledge of the factor structure; that is, the items that correspond to a factor 
#' (target items) are known. It is appropriate for target rotations in EFA/ESEM.
#'
#' @param data A matrix or data frame where rows represent items and columns represent factors. 
#'             Each value should be a standardized or pattern factor loading.
#' @param items_target A named list indicating the target items per factor. Each element should be a
#'                     numeric vector indicating the row indices (or item positions) expected to load
#'                     on the corresponding factor (column).
#'
#' @return A list containing three elements:
#' \describe{
#'   \item{TSFI}{A numeric value representing the factorial simplicity of the overall loading matrix.}
#'   \item{SFI}{A named vector with the factor scale fit index of each factor (column).}
#'   \item{IFS}{A data frame with two columns: \code{Items} (item names) and \code{IFS} 
#'                (index of factorial simplicity of each item).}
#' }
#'
#' @details
#' This function is designed for factorial solutions from models such as EFA with target rotation 
#' or ESEM. It requires a matrix or data frame of standardized or pattern loadings.
#' These levels of adjustment in the factorial matrix come from Fleming's approach for the SIMLOAD software (Fleming, 2003). 
#' Fleming (2003) proposes three levels of fit based on the degree of factorial simplicity: total, scale/factor and item. 
#' The item fit he derived from index of factorial simplicity (Kaiser, 1974); at the scale/factor level, factor scale fit 
#' index (SFI; Fleming, 1985, 2003); and at the total matrix, a derivated index from SFI. No like SIMLOAD software, here do
#' not calculate the Bentler Simplicity Index (BSI; Bentler, 1977).
#' 
#' @references
#' Bentler, P. M. (1977). Factor simplicity index and transformations. \emph{Psychometrika, 42}(2), 
#' 277–295. https://doi.org/10.1007/BF02294054
#' 
#' Fleming, J. S., & Merino Soto, C. (2005). Medidas de simplicidad y de ajuste factorial: 
#' un enfoque para la evaluación de escalas construidas factorialmente. \emph{Revista De Psicologia}, 
#' 23(2), 250–266. https://doi.org/10.18800/psico.200502.002
#' 
#' Fleming, J. S. (1985). An index of fit for factor scales. \emph{Educational and Psychological Measurement}, 45, 725–728.
#' https://doi.org/10.1177/0013164485454002
#' 
#' Kaiser, H. F. (1974). An index of factorial simplicity. \emph{Psychometrika}, 39, 31–35.
#' https://doi.org/10.1007/BF02291575
#' 
#' Fleming, J. S. (2003). Computing measures of simplicity of fit for loadings in factor-analytically 
#' derived scales. \emph{Behavior Research Methods, Instruments, & Computers}, 35(4), 520–524. 
#' https://doi.org/10.3758/bf03195531
#'
#' @examples
#' ##### Example 1 #####
#' ex1_fl <- data.frame(
#'   F1 = c(0.536, 0.708, 0.600, 0.673, 0.767, 0.481, -0.177, 0.209, -0.097, -0.115, 0.047, 0.024),
#'   F2 = c(-0.110, 0.026, 0.076, 0.011, -0.160, 0.106, 0.668, 0.438, 0.809, 0.167, 0.128, 0.041),
#'   F3 = c(-0.100, 0.036, 0.086, 0.021, -0.150, 0.116, 0.678, 0.448, 0.819, 0.577, 0.738, 0.751)
#' )
#' simload(data = ex1_fl, 
#'     items_target = list(F1 = c(1, 2, 3, 4, 5, 6),
#'                         F2 = c(7, 8, 9),
#'                         F3 = c(10, 11, 12)))
#'
#' ##### Example 2 #####
#' data(fullclean)
#'
#' INV.target <- matrix(0, 12, 2)
#' INV.target[1:6, 1] <- NA
#' INV.target[7:12, 2] <- NA
#'
#' INV.esem.model <- 'efa("efa1")*f1 + 
#' efa("efa1")*f2 =~ INV1 + INV4 + INV5 + INV7 + INV11 + 
#'                    INV12 + INV3 + INV6 + INV8 + INV9 + INV13 + INV14'
#'
#' INV.esem.fit <- lavaan::sem(INV.esem.model,
#'                              data = fullclean,
#'                              ordered = FALSE,
#'                              estimator = "ulsmv",
#'                              rotation = "target",
#'                              rotation.args = list(target = INV.target,
#'                                                   geomin.epsilon = 0.01,
#'                                                   rstarts = 30,
#'                                                   algorithm = "gpa",
#'                                                   std.ov = TRUE))
#'
#' simload(data = lavInspect(INV.esem.fit, what = "std")$lambda,
#'     items_target = list(f1 = c(1, 2, 3, 4, 5, 6),
#'                         f2 = c(7, 8, 9, 10, 11, 12)))
#'
#' 
#' @export
simload <- function(data, items_target) {
  
  # Convert matrices to data frame
  if (is.matrix(data)) {
    data <- as.data.frame(data)
  }
  
  # Verify that data is a data frame
  if (!is.data.frame(data)) {
    stop("The argument 'data' must be a data frame or a matrix")
  }
  
  # Get row names if available
  row_names <- rownames(data)
  if (is.null(row_names) || all(row_names == "")) {
    row_names <- paste0("FSI_", seq_len(nrow(data)))
  }
  
  # Check that factor names in items_target exist in data
  if (!all(names(items_target) %in% names(data))) {
    stop("All names in 'items_target' must match column names in 'data'.")
  }
  
  # Square all factor loadings
  data_squared <- data^2
  
  # Initialize lists to store results
  SFI <- list()
  IFS <- list()
  
  # Initialize totals for TSFI (Fleming-like denominator: total SSQ)
  SS_T_total  <- 0  # total sum of squares (all loadings in all columns)
  SS_NT_total <- 0  # non-target sum of squares (per target specification)
  
  # Ensure numeric indices are valid
  n_rows <- nrow(data_squared)
  
  for (factor in names(items_target)) {
    
    target_rows <- items_target[[factor]]
    
    if (any(target_rows < 1 | target_rows > n_rows)) {
      stop("One or more 'items_target' indices are out of range.")
    }
    
    # Rows that are non-target for this factor
    non_target_rows <- setdiff(seq_len(n_rows), target_rows)
    
    # SSQ for this factor (column)
    SS_T_j  <- sum(data_squared[, factor], na.rm = TRUE)                 # total (target + non-target)
    SS_NT_j <- sum(data_squared[non_target_rows, factor], na.rm = TRUE)  # non-target only
    
    # SFI_j (target-based adaptation, Fleming-like denominator)
    SFI[[factor]] <- if (SS_T_j == 0) NA_real_ else 1 - (SS_NT_j / SS_T_j)
    
    # Accumulate totals for TSFI
    SS_T_total  <- SS_T_total  + SS_T_j
    SS_NT_total <- SS_NT_total + SS_NT_j
    
    # IFS (kept as you implemented: item target vs cross-loadings on other factors)
    for (item in target_rows) {
      target_loading <- data_squared[item, factor]
      sum_non_target <- sum(data_squared[item, setdiff(names(data), factor)], na.rm = TRUE)
      
      IFS[[row_names[item]]] <- if (target_loading == 0) NA_real_ else 1 - (sum_non_target / target_loading)
    }
  }
  
  # TSFI (matrix-level: non-target SSQ / total SSQ)
  TSFI_total <- if (SS_T_total == 0) NA_real_ else 1 - (SS_NT_total / SS_T_total)
  
  # Output (same structure)
  result_list <- list(
    TSFI = round(TSFI_total, 3),
    SFI  = sapply(SFI, round, 3),
    IFS  = data.frame(
      Items = names(IFS),
      IFS   = round(unlist(IFS), 3),
      row.names = NULL
    )
  )
  
  return(result_list)
}