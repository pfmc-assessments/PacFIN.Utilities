test_that("'getArea()' returns a vector", {
  test_data <- data.frame(
    catch_area = paste0(4, letters),
    catch_area_new = "aaa"
  )
  test <- getArea(test_data, verbose = FALSE)
  expect_type(test, "character")
  expect_vector(test)
})

test_that("'getArea()' fails without an area column", {
  test_data <- data.frame(
    xx = paste0(4, letters),
    y = "aaa"
  )
  expect_error(getArea(test_data))
})

test_that("'getArea()' returns message if `verbose = TRUE`", {
  test_data <- data.frame(
    catch_area = paste0(1, letters),
    catch_area_new = "aaa"
  )
  expect_message(getArea(test_data, verbose = TRUE))
  expect_condition(getArea(test_data), regexp = NA)
  test_data <- data.frame(
    catch_area = paste0(4, letters),
    catch_area_new = "aaa"
  )
  expect_message(getArea(test_data, verbose = TRUE))
})
