test_that("plotFacomplex creates a ggplot object", {
  loadings <- data.frame(
    F1 = c(0.8, 0.7, 0.2, 0.1),
    F2 = c(0.1, 0.2, 0.7, 0.8)
  )
  items_target <- list(F1 = c(1, 2), F2 = c(3, 4))
  sim_result <- simload(loadings, items_target = items_target)
  
  # Ejecutar plotFacomplex (usando las columnas correctas)
  expect_error(p <- plotFacomplex(
    data = sim_result$IFS,
    item.col = "Items",
    value.col = "IFS"
  ), NA)
  
  # Verificar que devuelve un objeto ggplot
  expect_s3_class(p, "gg")
})