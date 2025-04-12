#' @title Plot Simplicity Index Values
#' @description
#' Creates a horizontal bar plot of item-level simplicity or complexity values (e.g., from FSI, BSI, Hofmann indices).
#' The user must specify the column name that contains the coefficient values (e.g., "FSI_i", "Complexity", etc.).
#'
#' @param data A data frame with item labels and simplicity or complexity values.
#' @param item.col Character. Name of the column with item labels. Default is \code{"Item"}.
#' @param value.col Character. Name of the column with the simplicity or complexity values. This argument is required.
#' @param sort.items Character. Sorting order of items: \code{"none"}, \code{"ascending"}, or \code{"descending"}. Default is \code{"ascending"}.
#' @param reverse.items Logical. If \code{TRUE}, reverses the order of items on the Y axis (top to bottom). Default is \code{FALSE}.
#' @param theme Character. ggplot2 theme to use: \code{"light"}, \code{"classic"}, or \code{"minimal"}. Default is \code{"light"}.
#' @param title Title of the plot. Default is \code{"Simplicity Index by Item"}.
#' @param bar.color Fill color for the bars. Default is \code{"blue"}.
#' @param threshold.line Optional numeric value. If specified, a horizontal dashed reference line is drawn at this threshold.
#' @param threshold.color Color for the threshold line and label. Default is \code{"red"}.
#'
#' @return A horizontal ggplot2 bar plot of item-level values.
#' @export
#'
#' @examples
#' # Example using FSI output:
#' fsi.out <- FSI(ex1_data)
#'
#' # Basic use (value.col is required)
#' plot.simplicity(
#'   data = fsi.out$FSI_i,
#'   item.col = "Items",
#'   value.col = "FSI_i"
#' )
#'
#' # Customizing options:
#' plot.simplicity(
#'   data = fsi.out$FSI_i,
#'   item.col = "Items",
#'   value.col = "FSI_i",
#'   sort.items = "none",
#'   reverse.items = TRUE,
#'   theme = "classic",
#'   bar.color = "darkgreen",
#'   threshold.line = 0.90
#' )
#'
#' # ❌ This will trigger an error if value.col is missing:
#' \dontrun{
#' plot.simplicity(data = fsi.out$FSI_i)  # Error: 'value.col' is required
#' }
plot.simplicity <- function(data,
                            item.col = "Item",
                            value.col = "Coefficient",
                            sort.items = c("ascending", "none", "descending"),
                            reverse.items = FALSE,
                            theme = c("light", "classic", "minimal"),
                            title = "Simplicity Index by Item",
                            bar.color = "blue",
                            threshold.line = NULL,
                            threshold.color = "red") {
  
  sort.items <- match.arg(sort.items)
  theme <- match.arg(theme)
  
  # Checks
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Please install the 'ggplot2' package.")
  if (!(item.col %in% names(data)) || !(value.col %in% names(data))) {
    stop(paste("The data.frame must contain columns:", item.col, "and", value.col))
  }
  
  # Determinar el orden base de los ítems según sort.items
  levels.base <- switch(
    sort.items,
    ascending  = data[[item.col]][order(data[[value.col]])],
    descending = data[[item.col]][order(-data[[value.col]])],
    none       = unique(data[[item.col]])
  )
  
  if (reverse.items) levels.base <- rev(levels.base)
  
  # Aplicar como factor
  data[[item.col]] <- factor(data[[item.col]], levels = levels.base)
  
  # Tema visual
  ggtheme <- switch(theme,
                    light   = ggplot2::theme_light(),
                    classic = ggplot2::theme_classic(),
                    minimal = ggplot2::theme_minimal()
  )
  
  # Crear gráfico
  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[item.col]], y = .data[[value.col]])) +
    ggplot2::geom_bar(stat = "identity", fill = bar.color) +
    ggplot2::coord_flip() +
    ggplot2::labs(title = title, x = "Items", y = "Simplicity Value") +
    ggtheme +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 10))
  
  # Línea de umbral
  if (!is.null(threshold.line)) {
    p <- p +
      ggplot2::geom_hline(yintercept = threshold.line, linetype = "dashed", color = threshold.color, size = 1) +
      ggplot2::annotate("text", x = 1, y = threshold.line,
                        label = paste("Threshold =", threshold.line),
                        vjust = -1, color = threshold.color)
  }
  
  print(p)
}
