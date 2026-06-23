test_that("simload returns correct structure", {
  loadings <- data.frame(
    F1 = c(0.8, 0.7, 0.2, 0.1),
    F2 = c(0.1, 0.2, 0.7, 0.8)
  )
  items_target <- list(F1 = c(1, 2), F2 = c(3, 4))
  
  result <- simload(loadings, items_target = items_target)
  
  # Verificar que es una lista
  expect_type(result, "list")
  
  # Verificar que contiene los elementos esperados
  expect_true(all(c("TSFI", "SFI", "IFS") %in% names(result)))
  
  # Verificar que IFS es un data.frame con las columnas correctas
  expect_s3_class(result$IFS, "data.frame")
  expect_true(all(c("Items", "IFS") %in% colnames(result$IFS)))
  
  # Verificar que el número de filas coincide
  expect_equal(nrow(result$IFS), nrow(loadings))
  
  # Verificar que TSFI y SFI son numéricos
  expect_type(result$TSFI, "double")
  expect_type(result$SFI, "double")
})