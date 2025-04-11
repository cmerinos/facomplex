#' @title Plot Simplicity Index Values
#' @description Creates a horizontal bar plot of simplicity index values for items, such as those produced by the \code{BSI()} function.
#'
#' @param data A data frame containing at least two columns: \code{Item} (item labels) and \code{BSI_Value} (numeric simplicity values).
#' @param title Optional character string. Title of the plot. Default is \code{"Simplicity Index by Item"}.
#' @param bar_color Character string indicating the fill color of the bars. Default is \code{"blue"}.
#' @param threshold_line Optional numeric value. If provided, a horizontal dashed reference line is added to the plot at this value.
#' @param threshold_color Character string for the color of the threshold line and its label. Default is \code{"red"}.
#'
#' @details
#' This function is intended for visualizing item-level simplicity indices such as the Bentler Simplicity Index (BSI),
#' or other complexity-related measures. It displays each item's value as a horizontal bar, optionally adding
#' a threshold reference line to help interpret cutoffs or target values.
#'
#' The function assumes that lower simplicity values reflect higher complexity, but makes no assumptions about thresholds —
#' the user defines them explicitly if needed.
#'
#' @seealso \code{\link{BSI}} to compute the Bentler Simplicity Index.
#'
#' @examples
#' # Using BSI results from example data
#' bsi_result <- BSI(ex1_data)
#'
#' # Plot the item-level BSI values
#' plot_simplicity(bsi_result$BSI_per_item)
#'
#' # With custom color and threshold line
#' plot_simplicity(bsi_result$BSI_per_item, bar_color = "green", threshold_line = 0.5, threshold_color = "black")
#'
#' @author Cesar Merino-Soto
#' 
#' @export
plot.simplicity <- function(data, title = "Índice de Simplicidad", bar_color = "blue", threshold_line = NULL) {
  # Validar si 'data' tiene las columnas correctas
  if (!("Item" %in% colnames(data)) || !("BSI_Value" %in% colnames(data))) {
    stop("El data.frame debe contener las columnas 'Item' y 'BSI_Value'.")
  }
  
  # Cargar ggplot2 si no está cargado
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    install.packages("ggplot2")
  }
  library(ggplot2)
  
  # Crear el gráfico
  p <- ggplot(data, aes(x = reorder(Item, BSI_Value), y = BSI_Value)) +
    geom_bar(stat = "identity", fill = bar_color) +
    coord_flip() +
    labs(title = title, x = "Ítems", y = "Valor del Índice") +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 10))
  
  # Si se define un threshold, agregar una línea de referencia
  if (!is.null(threshold_line)) {
    p <- p + geom_hline(yintercept = threshold_line, linetype = "dashed", color = "red", size = 1) +
      annotate("text", x = 1, y = threshold_line, label = paste("Threshold =", threshold_line), vjust = -1, color = "red")
  }
  
  print(p)  # Mostrar el gráfico
}
