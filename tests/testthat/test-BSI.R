test_that("BSI returns a numeric value between 0 and 1", {
  loadings <- matrix(c(
    0.8, 0.1,
    0.7, 0.2,
    0.2, 0.7,
    0.1, 0.8
  ), ncol = 2, byrow = TRUE)
  
  result <- BSI(loadings)
  
  # Debe ser numérico
  expect_type(result, "double")
  
  # Debe estar entre 0 y 1 (siempre que BSI esté normalizado, lo cual parece ser)
  expect_true(result >= 0 && result <= 1)
})