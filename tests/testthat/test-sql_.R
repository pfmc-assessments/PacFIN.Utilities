test_that("'sql_*()' returns a single character", {
  x <- c(sql_area, sql_species)
  test <- purrr::map_chr(.x = x, .f = rlang::exec)
  expect_type(test, "character")
  expect_length(test, length(x))

  x <- c(sql_bds, sql_catch)
  test <- purrr::map_chr(
    .x = x,
    .f = rlang::exec,
    pacfin_species_code = "PTRL"
  )
  expect_type(test, "character")
  expect_length(test, length(x))
  test <- purrr::map_chr(
    .x = x,
    .f = rlang::exec,
    pacfin_species_code = c("PTRL", "SABL")
  )
  expect_type(test, "character")
  expect_length(test, length(x))
})
