% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LSI.R
\name{LSI}
\alias{LSI}
\title{LSI - Loading Simplicity Index (Lorenzo-Seva, 2003)}
\usage{
LSI(loadings)
}
\arguments{
\item{loadings}{A matrix or data frame of factor loadings. Rows represent items, columns represent factors.}
}
\value{
A single numeric value representing the global simplicity index.
}
\description{
Computes the global Loading Simplicity Index (LSI) proposed by Lorenzo-Seva (2003), based on a matrix of factor loadings.
}
\details{
The Loading Simplicity Index (LSI) is a global measure of factor simplicity that captures how clearly
items load on a single factor across the entire loading matrix. The LSI captures the extent to which
each item's communality is concentrated on a single factor, emphasizing structural clarity across the matrix.
The index applies a nonlinear weighting function to emphasize dominant loadings while down-weighting smaller ones.
LSI ranges from 1 (perfectly simple structure) to 0 (fully complex structure). The index is designed to be
scale-invariant and interpretable regardless of the number of factors or items in the matrix.

The LSI is computed as:

\deqn{LSI = \frac{w - e}{1 - e}}

where \eqn{w} is the mean of weighted item complexity values, and \eqn{e} is the theoretical minimum
value of \eqn{w}, used for normalization. The result is scaled between 0 and 1, with higher values indicating
simpler structures.

\strong{Note:} Although the computation involves intermediate values per item (\eqn{w_i}), these should not
be interpreted individually. The LSI is validated and interpretable only as a global index.
}
\examples{
ex1_data <- data.frame(
  F1 = c(0.536, 0.708, 0.600, 0.673, 0.767, 0.481, -0.177, 0.209, -0.097, -0.115, 0.047, 0.024),
  F2 = c(-0.110, 0.026, 0.076, 0.011, -0.160, 0.106, 0.668, 0.438, 0.809, 0.167, 0.128, 0.041),
  F3 = c(-0.100, 0.036, 0.086, 0.021, -0.150, 0.116, 0.678, 0.448, 0.819, 0.577, 0.738, 0.751)
)

lsi <- LSI(ex1_data)
cat("Global LSI:", lsi, "\n")

}
\references{
Lorenzo-Seva, U. (2003). A factor simplicity index. \emph{Psychometrika, 68}(1), 49–60.
https://doi.org/10.1007/BF02296652
}
\seealso{
\code{\link{BSI}}, \code{\link{FSI}}, \code{\link{plot_simplicity}} for related indices and visualizations.

\code{\link{BSI}} for Bentler’s index, \code{\link{FSI}} for Fleming’s index, and \code{\link{plot_simplicity}}
for visual inspection of loadings.
}
\author{
Cesar Merino-Soto
}
