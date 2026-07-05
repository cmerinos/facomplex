#' KC: Kaiser-Cerny Simplicity Index and Ideal Hyperplane Count
#'
#' Computes the Kaiser-Cerny (1978) criterion for factorial simplicity based on a power function
#' of the absolute loadings (inspired by Kendall & Stuart, 1969). Also returns the ideal hyperplane count 
#' as an expected benchmark of factorial parsimony (Cattell, 1952).
#'
#' @param data A \code{data.frame} or numeric \code{matrix} of factor loadings, where rows represent items (variables) 
#' and columns represent factors.
#' @param b A positive numeric value for the power parameter in the Kaiser-Cerny formula. Default is \code{4}.
#' @param verbose Logical. If \code{TRUE} (default), prints results to the console. If \code{FALSE}, returns results silently.
#'
#' @details
#' The Kaiser-Cerny simplicity index is computed for each factor \code{j} using the formula:
#' \deqn{
#' f_j = \left( \frac{1}{m} \sum_{i=1}^{m} a_{ij}^{2/b} \right)^{b/2}
#' }
#' where \code{a_{ij}} is the loading of item \code{i} on factor \code{j}, and \code{m} is the number of items.
#' 
#' This index provides a quantitative assessment of factorial parsimony, where lower values of \code{f_j}
#' indicate a clearer hyperplane structure-meaning more loadings are close to zero-thus favoring simpler factor interpretation.
#' 
#' The function also reports the \emph{ideal hyperplane count}, defined as:
#' \deqn{
#' m(p - 1)
#' }
#' where \code{p} is the number of factors. This represents the theoretical number of near-zero loadings
#' required for a perfectly simple structure in factor analysis.
#'
#' @return
#' An object of class \code{"KC"} (invisibly if \code{verbose = TRUE}), which is a list containing:
#' \itemize{
#'   \item \code{fj}: A numeric vector with the Kaiser-Cerny simplicity index for each factor.
#'   \item \code{ideal_hyperplane_count}: The ideal hyperplane count.
#'   \item \code{b}: The power parameter used.
#'   \item \code{data}: The original input data (for reference).
#' }
#'
#' @references
#' Cattell, R. B. (1952). Factor analysis: an introduction and manual for the psychologist and social scientist. Oxford, 
#' England: Harper.
#' Kaiser, H. F., & Cerny, B. A. (1978). Casey's Method For Fitting Hyperplanes From An Intermediate Orthomax Solution. 
#' \emph{Multivariate Behavioral Research}, 13(4), 395-401. https://doi.org/10.1207/s15327906mbr1304_2
#' 
#' Kendall, M. G., & Stuart, A. (1969). \emph{The Advanced Theory of Statistics}, Vol. 2. London: Griffin.
#'
#' @examples
#' # Simulated example
#' \donttest{set.seed(123)
#' loadings <- matrix(runif(30, -1, 1), nrow = 10, ncol = 3)
#' 
#' res <- KC(loadings)
#' 
#' res  # print the results
#' }
#'
#' @export
KC <- function(data, b = 4, verbose = TRUE) {
  
  # Ensure numeric matrix
  if (!is.matrix(data)) {
    data <- as.matrix(data)
  }
  
  if (b <= 0) {
    stop("The parameter 'b' must be a positive number.")
  }
  
  m <- nrow(data)
  p <- ncol(data)
  
  # Compute f_j for each factor
  fj_values <- apply(data^2, 2, function(factor) {
    term <- sum(factor^(1 / b)) / m
    term^(b / 2)
  })
  
  ideal_hyperplane_count <- m * (p - 1)
  
  # Prepare the output object (invisible)
  out <- list(
    fj = fj_values,
    ideal_hyperplane_count = ideal_hyperplane_count,
    b = b,
    data = data
  )
  class(out) <- "KC"
  
  # Print if verbose
  if (verbose) {
    print(out)
  }
  
  # Return invisibly so the object can be assigned
  invisible(out)
}

#' Print method for KC objects
#'
#' @param x An object of class \code{"KC"}.
#' @param ... Additional arguments (not used).
#' @export
print.KC <- function(x, ...) {
  cat("Kaiser-Cerny Factor Simplicity Analysis:\n")
  cat("- Threshold f_j for hyperplane inclusion (per factor):\n")
  for (i in seq_along(x$fj)) {
    cat(paste0("  F", i, ": ", round(x$fj[i], 6), "\n"))
  }
  cat("\n- Ideal hyperplane count: ", x$ideal_hyperplane_count, "\n")
  invisible(x)
}