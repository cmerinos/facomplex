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
