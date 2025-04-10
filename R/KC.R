#' KC: Kaiser-Cerny Simplicity Index and Ideal Hyperplane Count
#'
#' Computes the Kaiser-Cerny (1978) criterion for factorial simplicity based on a power function
#' of the absolute loadings (inspired by Kendall & Stuart, 1969). Also returns the ideal hyperplane count 
#' as an expected benchmark of factorial parsimony.
#'
#' @param data A \code{data.frame} or numeric \code{matrix} of factor loadings, where rows represent items (variables) 
#' and columns represent factors.
#' @param b A positive numeric value for the power parameter in the Kaiser-Cerny formula. Default is \code{4}.
#'
#' @details
#' The Kaiser-Cerny simplicity index is computed for each factor \code{j} using the formula:
#' \deqn{
#' f_j = \left( \frac{1}{m} \sum_{i=1}^{m} a_{ij}^{2/b} \right)^{b/2}
#' }
#' where \code{a_{ij}} is the loading of item \code{i} on factor \code{j}, and \code{m} is the number of items.
#' 
#' This index provides a quantitative assessment of factorial parsimony, where lower values of \code{f_j}
#' indicate a clearer hyperplane structure—meaning more loadings are close to zero—thus favoring simpler factor interpretation.
#' 
#' The function also reports the \emph{ideal hyperplane count}, defined as:
#' \deqn{
#' m(p - 1)
#' }
#' where \code{p} is the number of factors. This represents the theoretical number of near-zero loadings
#' required for a perfectly simple structure in factor analysis.
#'
#' @return No return value. The function prints:
#' \itemize{
#'   \item The Kaiser-Cerny simplicity index \code{f_j} for each factor.
#'   \item The ideal hyperplane count \code{m(p - 1)}.
#' }
#'
#' @references
#' Kaiser, H. F., & Cerny, B. A. (1978). Factor analysis of the image covariance matrix. \emph{Psychological Bulletin}, 85(6), 1272–1284.  
#' Kendall, M. G., & Stuart, A. (1969). \emph{The Advanced Theory of Statistics}, Vol. 2. London: Griffin.
#'
#' @examples
#' # Simulated example
#' set.seed(123)
#' loadings <- matrix(runif(30, -1, 1), nrow = 10, ncol = 3)
#' KC(loadings)
#'
#' @export
KC <- function(data, b = 4) {
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
  
  # Output results
  cat("Kaiser-Cerny Factor Simplicity Analysis:\n")
  cat("- Simplicity index f_j for each factor:\n")
  for (i in 1:length(fj_values)) {
    cat(paste0("  F", i, ": ", round(fj_values[i], 6), "\n"))
  }
  cat("\n- Ideal hyperplane count: ", ideal_hyperplane_count, "\n")
}
